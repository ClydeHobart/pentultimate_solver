mod uses {
	pub use {
		crate::{
			app::prelude::*,
			prelude::*,
			puzzle::{
				consts::*,
				transformation::{
					Action,
					HalfAddr,
					HalfAddrAttrs
				},
				InflatedPuzzleState,
				InflatedPieceStateComponent
			},
			ui::input::{
				PendingActions,
				PuzzleAction,
				PuzzleActionType
			},
			util::{
				inspectable_bit_array::InspectableBitArray,
				inspectable_num::InspectableNum
			}
		},
		bevy::prelude::*,
		bevy_inspector_egui::{
			options::NumberAttributes,
			Context,
			Inspectable
		},
		bit_field::{
			BitArray,
			BitField
		},
		egui::{
			widgets::Separator,
			Button,
			Color32,
			Ui,
			Vec2
		},
		num_traits::One,
		serde::{
			Deserialize,
			Deserializer
		},
		std::{
			any::Any,
			convert::TryFrom,
			mem::transmute,
			ops::{
				Deref,
				Range
			}
		}
	};
}

pub mod prelude {
	pub use super::{
		data::{
			ToolDataBox,
			Stack
		},
		Tool
	};
}

use self::{
	uses::*,
	data::*
};

type ToolInner = u8;

macro_rules! define_tool {
	($($tool:ident),*) => {
		#[derive(Clone, Copy, Deserialize, PartialEq)]
		pub enum Tool {
			$(
				$tool,
			)*
		}

		const fn tool_count() -> usize {
			#![allow(path_statements)]

			let mut count: usize = 0_usize;

			$(
				Tool::$tool;
				count += 1_usize;
			)*

			count
		}

		impl Tool {
			fn as_str(self) -> &'static str {
				const TOOL_STRS: [&str; TOOL_COUNT] = [
					$(
						stringify!($tool),
					)*
				];

				TOOL_STRS[self as usize]
			}

			fn new_data(self) -> ToolDataBox {
				match self {
					$(
						Self::$tool => Box::new(<data::$tool>::default()) as ToolDataBox,
					)*
				}
			}

			fn data_eq_impl(lhs: &dyn ToolData, rhs: &dyn ToolData) -> bool {
				if lhs.tool() == rhs.tool() {
					let lhs_any: &dyn Any = lhs.as_any();
					let rhs_any: &dyn Any = rhs.as_any();

					match lhs.tool() {
						$(
							Tool::$tool => {
								let lhs_data: Option<&$tool> = lhs_any.downcast_ref::<$tool>();
								let rhs_data: Option<&$tool> = rhs_any.downcast_ref::<$tool>();

								lhs_data.is_some() && rhs_data.is_some() && *lhs_data.unwrap() == *rhs_data.unwrap()
							},
						)*
					}
				} else {
					false
				}
			}
		}

		$(
			impl ToolData for $tool {
				fn tool(&self) -> Tool { Tool::$tool }

				fn clone_impl(&self) -> ToolDataBox { Box::new(self.clone()) as ToolDataBox }

				fn as_tool_data(&self) -> &dyn ToolData { self }

				fn as_any(&self) -> &dyn Any { self }
			}
		)*
	};
}

define_tool!(
	PuzzleState,
	Stack
);

const TOOL_COUNT: usize = tool_count();

const_assert!(TOOL_COUNT <= ToolInner::MAX as usize);

mod data {
	use crate::puzzle::transformation::Addr;

	use super::{
		uses::*,
		Tool
	};

	// We need this trait to be able to restrict ToolData with it while still have ToolDataBox still compile, since Inspectable::setup() has no self argument
	pub trait InvokeUi {
		fn invoke_ui(&mut self, ui: &mut Ui, context: &mut Context) -> bool;
	}

	impl<T: Inspectable<Attributes = ()>> InvokeUi for T {
		fn invoke_ui(&mut self, ui: &mut Ui, context: &mut Context) -> bool { self.ui(ui, (), context) }
	}

	pub trait ToolData: InvokeUi {
		fn tool(&self) -> Tool;

		fn clone_impl(&self) -> ToolDataBox;

		fn as_tool_data(&self) -> &dyn ToolData;

		fn as_any(&self) -> &dyn Any;

		fn eq_impl(&self, other: &dyn ToolData) -> bool {
			Tool::data_eq_impl(self.as_tool_data(), other)
		}
	}

	pub type ToolDataBox = Box<dyn ToolData + Send + Sync>;

	impl Clone for ToolDataBox {
		fn clone(&self) -> Self { self.clone_impl() }
	}

	impl PartialEq for ToolDataBox {
		fn eq(&self, other: &ToolDataBox) -> bool {
			Tool::data_eq_impl(&**self, &**other)
		}
	}

	#[derive(Clone, Default, Inspectable, PartialEq)]
	pub struct StatsOptions {
		reorientation:			HalfAddr,
		#[inspectable(collapse, length = PENTAGON_SIDE_COUNT)]
		desired_pent_rot_sums:	InspectableBitArray<u8, 1_usize>,
		#[inspectable(collapse, length = TRIANGLE_SIDE_COUNT)]
		desired_tri_rot_sums:	InspectableBitArray<u8, 1_usize>
	}

	#[derive(Clone, Default, PartialEq)]
	pub struct PuzzleState {
		stats_options: StatsOptions
	}

	impl Inspectable for PuzzleState {
		type Attributes = ();

		fn ui(&mut self, ui: &mut Ui, _: (), context: &mut Context) -> bool {
			if context.world().is_none() {
				return false;
			}

			context.world_scope(ui, "", |world: &mut World, ui: &mut Ui, context: &mut Context| -> bool {
				if let Some(extended_puzzle_state) = world.get_resource::<ExtendedPuzzleState>() {
					let mut changed: bool = false;

					ui.collapsing("Position & Rotation", |ui: &mut Ui| -> () {
						ui.monospace(format!("{:#?}", extended_puzzle_state.puzzle_state));
					});
					ui.collapsing("Stats", |ui: &mut Ui| -> () {
						changed |= self.stats_options.ui(ui, (), context);

						egui::Grid::new("StatsTable").show(ui, |ui: &mut Ui| -> () {
							let InflatedPuzzleState {
								pos,
								rot
							} = if self.stats_options.reorientation.is_valid() {
								&extended_puzzle_state.puzzle_state
									+ self.stats_options.reorientation.as_reorientation()
							} else {
								extended_puzzle_state.puzzle_state.clone()
							};

							let mut correct_pent_pos_count:	u32 = 0_u32;
							let mut correct_tri_pos_count:	u32 = 0_u32;
							let mut correct_pent_rot_count:	u32 = 0_u32;
							let mut correct_tri_rot_count:	u32 = 0_u32;
							let mut pent_rot_sum:			InflatedPieceStateComponent = ZERO_IPSC;
							let mut tri_rot_sum:			InflatedPieceStateComponent = ZERO_IPSC;

							for pent_index in PENTAGON_PIECE_RANGE {
								correct_pent_pos_count += (pos[pent_index] as usize == pent_index) as u32;
								correct_pent_rot_count += (rot[pent_index] == ZERO_IPSC) as u32;
								pent_rot_sum += rot[pent_index];
							}

							pent_rot_sum %= PENTAGON_SIDE_COUNT_IPSC;

							for tri_index in TRIANGLE_PIECE_RANGE {
								correct_tri_pos_count += (pos[tri_index] as usize == tri_index) as u32;
								correct_tri_rot_count += (rot[tri_index] == ZERO_IPSC) as u32;
								tri_rot_sum += rot[tri_index];
							}

							tri_rot_sum %= TRIANGLE_SIDE_COUNT_IPSC;

							let desired_pent_rot_sum: bool = self
								.stats_options
								.desired_pent_rot_sums
								.0
								.get_bit(pent_rot_sum as usize);
							let desired_tri_rot_sum: bool = self
								.stats_options
								.desired_tri_rot_sums
								.0
								.get_bit(tri_rot_sum as usize);
							let desired_rot_sum: bool = desired_pent_rot_sum && desired_tri_rot_sum;

							macro_rules! colored_label {
								($count:expr, $max:ident) => {
									let count: InflatedPieceStateComponent = $count;

									ui.colored_label(
										Color32::from_alt(red_to_green(count as f32 / $max)),
										format!("{}", count)
									);
								}
							}

							ui.label("");
							ui.label("Pents").on_hover_text("Pentagon Pieces");
							ui.label("Tris").on_hover_text("Triangle Pieces");
							ui.label("Total").on_hover_text("All Pieces");
							ui.end_row();

							ui.label("Correct Pos").on_hover_text("Correct Position Count");
							colored_label!(correct_pent_pos_count, PENTAGON_PIECE_COUNT_F32);
							colored_label!(correct_tri_pos_count, TRIANGLE_PIECE_COUNT_F32);
							colored_label!(correct_pent_pos_count + correct_tri_pos_count, PIECE_COUNT_F32);
							ui.end_row();

							ui.label("Correct Rot").on_hover_text("Correct Rotation Count");
							colored_label!(correct_pent_rot_count, PENTAGON_PIECE_COUNT_F32);
							colored_label!(correct_tri_rot_count, TRIANGLE_PIECE_COUNT_F32);
							colored_label!(correct_pent_rot_count + correct_tri_rot_count, PIECE_COUNT_F32);
							ui.end_row();

							ui.label("Rot Sum").on_hover_text("Rotation Sum (% Piece Side Count)");
							ui.colored_label(
								Color32::from_alt(red_to_green(desired_pent_rot_sum as u8 as f32)),
								format!("{}", pent_rot_sum)
							);
							ui.colored_label(
								Color32::from_alt(red_to_green(desired_tri_rot_sum as u8 as f32)),
								format!("{}", tri_rot_sum)
							);
							ui.colored_label(
								Color32::from_alt(red_to_green(desired_rot_sum as u8 as f32)),
								format!("{}", if desired_rot_sum {
									"✔"
								} else {
									"❌"
								})
							);
							ui.end_row();
						});
					});

					changed
				} else {
					false
				}
			})
		}
	}

	#[derive(Clone, Default, PartialEq)]
	pub struct Stack {
		pub min_focus_index:	usize,
		pub max_focus_index:	usize,
		debug_addr:				HalfAddr,
		pub print_true_action:	bool
	}

	impl Stack {
		fn focus_range(&self) -> Range<usize> {
			self.min_focus_index .. self.max_focus_index + 1_usize
		}
	}

	impl Inspectable for Stack {
		type Attributes = ();

		fn ui(&mut self, ui: &mut Ui, _: (), context: &mut Context) -> bool {
			let mut changed: bool = false;

			ui.collapsing("Focus Indices", |ui: &mut Ui| -> () {
				egui::Grid::new(context.id())
					.min_col_width(0.0_f32)
					.show(ui, |ui: &mut Ui| -> () {
						ui.label("Min");
						ui.add(Separator::default().vertical());
						changed = <&mut InspectableNum<usize>>::from(&mut self.min_focus_index).ui(
							ui,
							NumberAttributes::<usize>::between(
								0_usize,
								self.max_focus_index
							),
							&mut context.with_id(0_u64)
						);
						ui.end_row();
						ui.label("Max");
						ui.add(Separator::default().vertical());

						let max_focus_index: usize = self.max_focus_index;

						changed |= <&mut InspectableNum<usize>>::from(&mut self.max_focus_index).ui(
							ui,
							NumberAttributes::<usize>::between(
								self.min_focus_index,
								context
									.world()
									.and_then(|world: &World| -> Option<usize> {
										world
											.get_resource::<ExtendedPuzzleState>()
											.map(|extended_puzzle_state: &ExtendedPuzzleState| -> usize {
												extended_puzzle_state.actions.len().max(1_usize) - 1_usize
											})
									})
									.unwrap_or(max_focus_index)
							),
							&mut context.with_id(1_u64)
						);
						ui.end_row();
					});
			});
			ui.collapsing("Debug Addr", |ui: &mut Ui| -> () {
				self.debug_addr.ui(ui, HalfAddrAttrs::default(), &mut context.with_id(2_u64));
			});
			ui.horizontal(|ui: &mut Ui| -> () {
				ui.label("Print True Action");
				self.print_true_action.ui(ui, (), &mut context.with_id(3_u64));
			});
			ui.separator();

			let mut world: Option<&mut World> = unsafe { context.world_mut() };
			let focus_range: Range<usize> = self.focus_range();
			let (can_simplify_actions, can_print_actions, can_set_camera_start, can_reorient_actions):
				(bool, bool, bool, bool) =
				world
					.as_ref()
					.and_then(|world: &&mut World| -> Option<(&InputState, &ExtendedPuzzleState)> {
						world
							.get_resource::<InputState>()
							.filter(|input_state: &&InputState| -> bool {
								!input_state.has_active_action()
							})
							.zip(world.get_resource::<ExtendedPuzzleState>())
					})
					.map(|(_, extended_puzzle_state): (&InputState, &ExtendedPuzzleState)| -> (bool, bool, bool, bool) {
						let debug_addr_is_valid: bool = self.debug_addr.is_valid();

						(
							extended_puzzle_state.can_simplify_actions(&focus_range),
							extended_puzzle_state.actions_are_simplified(&focus_range),
							debug_addr_is_valid,
							debug_addr_is_valid && extended_puzzle_state.can_reorient_actions(&focus_range)
						)
					})
					.map(|
						(can_simplify_actions, can_print_actions, can_set_camera_start, can_reorient_actions):
						(bool, bool, bool, bool)
					| -> (bool, bool, bool, bool) {
						(
							can_simplify_actions,
							can_print_actions,
							can_set_camera_start,
							can_reorient_actions && {
								/* this part needs to be in its own map() call because immutable resources are still in
								scope during the first call */
								let world: &mut World = world.as_mut().unwrap();

								world.contains_resource::<Preferences>()
									&& world.query::<CameraTuple>().iter(world).next().is_some()
							}
						)
					})
					.unwrap_or_default();

			if ui.add_enabled(can_simplify_actions, Button::new("Simplify Actions")).clicked() {
				world
					.as_mut()
					.unwrap()
					.get_resource_mut::<ExtendedPuzzleState>()
					.unwrap()
					.simplify_actions(&focus_range);
				changed = true;
			}

			if ui.add_enabled(can_print_actions, Button::new("Print Actions")).clicked() {
				let comprising_simples: Option<Vec<HalfAddr>> = world
					.as_ref()
					.unwrap()
					.get_resource::<ExtendedPuzzleState>()
					.unwrap()
					.get_as_comprising_simples(&focus_range);

				if let Some(comprising_simples) = comprising_simples {
					log::debug!(
						"Print Actions:\n{:?}",
						comprising_simples
							.iter()
							.map(|simple: &HalfAddr| -> (u32, u32) {
								(simple.get_species_index() as u32, simple.get_organism_index() as u32)
							})
							.collect::<Vec<(u32, u32)>>()
					);
				} else {
					log::debug!("Print Actions: couldn't obtain comprising simples");
				}
			}

			if ui.add_enabled(can_set_camera_start, Button::new("Set Camera Start")).clicked() {
				world
					.as_mut()
					.unwrap()
					.get_resource_mut::<ExtendedPuzzleState>()
					.unwrap()
					.set_camera_start(&focus_range, self.debug_addr);
				changed = true;
			}

			if ui
				.add_enabled(can_set_camera_start, Button::new("Set Initial Camera Start"))
				.clicked()
			{
				world
					.as_mut()
					.unwrap()
					.get_resource_mut::<ExtendedPuzzleState>()
					.unwrap()
					.set_initial_camera_start(&focus_range, self.debug_addr);
				changed = true;
			}

			if ui.add_enabled(can_reorient_actions, Button::new("Reorient Actions")).clicked() {
				let puzzle_action: Option<PuzzleAction> = {
					let world: &mut World = world.as_mut().unwrap();

					Some(
						PuzzleAction {
							current_action:		None,
							pending_actions:	Some(Box::new({
								let mut extended_puzzle_state: ExtendedPuzzleState = world
									.get_resource::<ExtendedPuzzleState>()
									.unwrap()
									.clone();
								let mut camera: HalfAddr = CameraQueryStateNT(&mut (world.query::<CameraTuple>()))
									.orientation(world, |camera_orientation: Option<&Quat>| -> HalfAddr {
										camera_orientation
											.map_or(
												HalfAddr::default(),
												CameraPlugin::compute_camera_addr
											)
									});

								extended_puzzle_state.reorient_actions(
									&focus_range,
									self.debug_addr,
									&mut camera
								);

								PendingActions::load(
									&SaveState {
										extended_puzzle_state,
										input_toggles: world.get_resource::<InputState>().unwrap().toggles.clone(),
										camera,
									},
									&world.get_resource::<Preferences>().unwrap().speed.animation
								)
							})),
							action_type: PuzzleActionType::Load
						}
					)
				};

				world.as_mut().unwrap().get_resource_mut::<InputState>().unwrap().puzzle_action = puzzle_action;
			}



			changed
		}
	}
}

impl TryFrom<usize> for Tool {
	type Error = usize;

	fn try_from(value: usize) -> Result<Self, Self::Error> {
		if value < TOOL_COUNT {
			Ok(unsafe { transmute::<ToolInner, Self>(value as ToolInner) })
		} else {
			// Return how much out of range the value is
			Err(value - TOOL_COUNT + 1_usize)
		}
	}
}

type ToolsInner = u32;

#[derive(Clone, Default)]
pub struct ToolsData {
	active_tools:		ToolsInner,
	tool_data_boxes:	Vec<ToolDataBox>
}

const_assert!(TOOL_COUNT <= ToolsInner::BITS as usize);

impl ToolsData {
	pub fn should_render(&self) -> bool { !self.tool_data_boxes.is_empty() }

	pub fn is_tool_active(&self, tool: Tool) -> bool {
		self.active_tools.get_bit(tool as usize)
	}

	pub fn get_tool_data(&self, tool: Tool) -> Option<&ToolDataBox> {
		if self.is_tool_active(tool) {
			Some(&self.tool_data_boxes[
				(self.active_tools & ((ToolsInner::one() << tool as u32) - ToolsInner::one()))
					.count_ones() as usize
			])
		} else {
			None
		}
	}

	pub fn render(&mut self, ui: &mut Ui, context: &mut Context) -> () {
		egui::CollapsingHeader::new("Tools")
			.default_open(true)
			.show(ui, |ui: &mut Ui| -> () {
				let mut tool_data_index: usize = 0_usize;

				for tool_index in 0_usize .. TOOL_COUNT {
					let tool: Tool = Tool::try_from(tool_index).unwrap();

					if self.is_tool_active(tool) {
						ui.collapsing(
							tool.as_str(),
							|ui: &mut Ui| -> () {
								self
									.tool_data_boxes
									[tool_data_index]
									.invoke_ui(ui, &mut context.with_id(tool_data_index as u64));
							}
						);
						tool_data_index += 1_usize;
					}
				}
			});
	}
}

impl<'de> Deserialize<'de> for ToolsData {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		let mut tools: Self = <Self as Default>::default();

		for tool in Vec::<Tool>::deserialize(deserializer)? {
			tools.active_tools.set_bit(tool as usize, true);
		}

		for tool_index in 0_usize .. TOOL_COUNT {
			if tools.active_tools.get_bit(tool_index) {
				tools.tool_data_boxes.push(Tool::try_from(tool_index).unwrap().new_data());
			}
		}

		Ok(tools)
	}
}

impl Inspectable for ToolsData {
	type Attributes = ();

	fn ui(&mut self, ui: &mut Ui, _: (), _: &mut Context) -> bool {
		let mut changed: bool = false;
		let mut tool_data_index: usize = 0_usize;

		for tool_index in 0_usize .. TOOL_COUNT {
			let tool: Tool = Tool::try_from(tool_index).unwrap();
			let mut tool_bool: bool = self.active_tools.get_bit(tool_index);

			if ui.checkbox(&mut tool_bool, tool.as_str()).changed() {
				self.active_tools.set_bit(tool_index, tool_bool);

				if tool_bool {
					self
						.tool_data_boxes
						.insert(
							tool_data_index,
							tool.new_data()
						);
				} else {
					self.tool_data_boxes.remove(tool_data_index);
				}

				changed = true;
			}

			if tool_bool {
				tool_data_index += 1_usize;
			}
		}

		changed
	}
}

impl PartialEq for ToolsData {
	fn eq(&self, other: &Self) -> bool {
		if self.active_tools == other.active_tools {
			for (
				self_tool_data,
				other_tool_data
			) in self.tool_data_boxes.iter().zip(other.tool_data_boxes.iter()) {
				if self_tool_data != other_tool_data {
					return false;
				}
			}

			true
		} else {
			false
		}
	}
}