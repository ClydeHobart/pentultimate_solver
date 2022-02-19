mod uses {
	pub use {
		crate::{
			app::prelude::*,
			prelude::*,
			puzzle::{
				consts::*,
				transformation::HalfAddr,
				InflatedPuzzleState,
				InflatedPieceStateComponent
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
			mem::transmute
		}
	};
}

pub mod prelude {
	pub use super::{
		data::{
			DebugModeDataBox,
			Stack
		},
		DebugMode
	};
}

use self::{
	uses::*,
	data::*
};

type DebugModeInner = u8;

macro_rules! define_debug_mode {
	($($debug_mode:ident),*) => {
		#[derive(Clone, Copy, Deserialize, PartialEq)]
		pub enum DebugMode {
			$(
				$debug_mode,
			)*
		}

		const fn debug_mode_count() -> usize {
			#![allow(path_statements)]

			let mut count: usize = 0_usize;

			$(
				DebugMode::$debug_mode;
				count += 1_usize;
			)*

			count
		}

		impl DebugMode {
			fn as_str(self) -> &'static str {
				const DEBUG_MODE_STRS: [&str; DEBUG_MODE_COUNT] = [
					$(
						stringify!($debug_mode),
					)*
				];

				DEBUG_MODE_STRS[self as usize]
			}

			fn new_data(self) -> DebugModeDataBox {
				match self {
					$(
						Self::$debug_mode => Box::new(<data::$debug_mode>::default()) as DebugModeDataBox,
					)*
				}
			}

			fn data_eq_impl(lhs: &dyn DebugModeData, rhs: &dyn DebugModeData) -> bool {
				if lhs.debug_mode() == rhs.debug_mode() {
					let lhs_any: &dyn Any = lhs.as_any();
					let rhs_any: &dyn Any = rhs.as_any();

					match lhs.debug_mode() {
						$(
							DebugMode::$debug_mode => {
								let lhs_data: Option<&$debug_mode> = lhs_any.downcast_ref::<$debug_mode>();
								let rhs_data: Option<&$debug_mode> = rhs_any.downcast_ref::<$debug_mode>();

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
			impl DebugModeData for $debug_mode {
				fn debug_mode(&self) -> DebugMode { DebugMode::$debug_mode }

				fn clone_impl(&self) -> DebugModeDataBox { Box::new(self.clone()) as DebugModeDataBox }

				fn as_debug_mode_data(&self) -> &dyn DebugModeData { self }

				fn as_any(&self) -> &dyn Any { self }
			}
		)*
	};
}

define_debug_mode!(
	PuzzleState,
	Stack
);

const DEBUG_MODE_COUNT: usize = debug_mode_count();

const_assert!(DEBUG_MODE_COUNT <= DebugModeInner::MAX as usize);

mod data {
	use super::{
		uses::*,
		DebugMode
	};

	// We need this trait to be able to restrict DebugModeData with it while still have DebugModeDataBox still compile,
	// since Inspectable::setup() has no self argument
	pub trait InvokeUi {
		fn invoke_ui(&mut self, ui: &mut Ui, context: &mut Context) -> bool;
	}

	impl<T: Inspectable<Attributes = ()>> InvokeUi for T {
		fn invoke_ui(&mut self, ui: &mut Ui, context: &mut Context) -> bool { self.ui(ui, (), context) }
	}

	pub trait DebugModeData: InvokeUi {
		fn debug_mode(&self) -> DebugMode;

		fn clone_impl(&self) -> DebugModeDataBox;

		fn as_debug_mode_data(&self) -> &dyn DebugModeData;

		fn as_any(&self) -> &dyn Any;

		fn eq_impl(&self, other: &dyn DebugModeData) -> bool {
			DebugMode::data_eq_impl(self.as_debug_mode_data(), other)
		}
	}

	pub type DebugModeDataBox = Box<dyn DebugModeData + Send + Sync>;

	impl Clone for DebugModeDataBox {
		fn clone(&self) -> Self { self.clone_impl() }
	}

	impl PartialEq for DebugModeDataBox {
		fn eq(&self, other: &DebugModeDataBox) -> bool {
			DebugMode::data_eq_impl(&**self, &**other)
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
		pub min_simplification_index:	InspectableNum<usize>,
		pub max_simplification_index:	InspectableNum<usize>
	}

	impl Inspectable for Stack {
		type Attributes = ();

		fn ui(&mut self, ui: &mut Ui, _: (), context: &mut Context) -> bool {
			let mut changed: bool = false;

			ui.collapsing("simplification_indices", |ui: &mut Ui| -> () {
				egui::Grid::new(context.id())
					.min_col_width(0.0_f32)
					.show(ui, |ui: &mut Ui| -> () {
						ui.label("min");
						ui.add(Separator::default().vertical());
						changed = self.min_simplification_index.ui(
							ui,
							NumberAttributes::<usize>::between(
								0_usize,
								self.max_simplification_index.0
							),
							context
						);
						ui.end_row();
						ui.label("max");
						ui.add(Separator::default().vertical());
						changed |= self.max_simplification_index.ui(
							ui,
							NumberAttributes::<usize>::between(
								self.min_simplification_index.0,
								context
									.world()
									.and_then(|world: &World| -> Option<usize> {
										world
											.get_resource::<ExtendedPuzzleState>()
											.map(|extended_puzzle_state: &ExtendedPuzzleState| -> usize {
												extended_puzzle_state.actions.len().max(1_usize) - 1_usize
											})
									})
									.unwrap_or(self.max_simplification_index.0)
							),
							context
						);
						ui.end_row();
					});
			});

			false
		}
	}
}

impl TryFrom<usize> for DebugMode {
	type Error = usize;

	fn try_from(value: usize) -> Result<Self, Self::Error> {
		if value < DEBUG_MODE_COUNT {
			Ok(unsafe { transmute::<DebugModeInner, Self>(value as DebugModeInner) })
		} else {
			// Return how much out of range the value is
			Err(value - DEBUG_MODE_COUNT + 1_usize)
		}
	}
}

type DebugModesInner = u32;

#[derive(Clone, Default)]
pub struct DebugModes {
	active_debug_modes:	DebugModesInner,
	debug_mode_data:	Vec<DebugModeDataBox>
}

const_assert!(DEBUG_MODE_COUNT <= DebugModesInner::BITS as usize);

impl DebugModes {
	pub fn should_render(&self) -> bool { !self.debug_mode_data.is_empty() }

	pub fn is_debug_mode_active(&self, debug_mode: DebugMode) -> bool {
		self.active_debug_modes.get_bit(debug_mode as usize)
	}

	pub fn get_debug_mode_data(&self, debug_mode: DebugMode) -> Option<&DebugModeDataBox> {
		if self.is_debug_mode_active(debug_mode) {
			Some(&self.debug_mode_data[
				(self.active_debug_modes & ((DebugModesInner::one() << debug_mode as u32) - DebugModesInner::one()))
					.count_ones() as usize
			])
		} else {
			None
		}
	}

	pub fn render(&mut self, ui: &mut Ui, context: &mut Context) -> () {
		egui::CollapsingHeader::new("Debug Modes")
			.default_open(true)
			.show(ui, |ui: &mut Ui| -> () {
				let mut debug_mode_data_index: usize = 0_usize;

				for debug_mode_index in 0_usize .. DEBUG_MODE_COUNT {
					let debug_mode: DebugMode = DebugMode::try_from(debug_mode_index).unwrap();

					if self.is_debug_mode_active(debug_mode) {
						ui.collapsing(
							debug_mode.as_str(),
							|ui: &mut Ui| -> () {
								self
									.debug_mode_data
									[debug_mode_data_index]
									.invoke_ui(ui, &mut context.with_id(debug_mode_data_index as u64));
							}
						);
						debug_mode_data_index += 1_usize;
					}
				}
			});
	}

	pub fn default() -> Self { Self::from_file_or_default(STRING_DATA.debug.debug_modes.as_str()) }
}

impl<'de> Deserialize<'de> for DebugModes {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		let mut debug_modes: Self = <Self as Default>::default();

		for debug_mode in Vec::<DebugMode>::deserialize(deserializer)? {
			debug_modes.active_debug_modes.set_bit(debug_mode as usize, true);
		}

		for debug_mode_index in 0_usize .. DEBUG_MODE_COUNT {
			if debug_modes.active_debug_modes.get_bit(debug_mode_index) {
				debug_modes.debug_mode_data.push(DebugMode::try_from(debug_mode_index).unwrap().new_data());
			}
		}

		Ok(debug_modes)
	}
}

impl Inspectable for DebugModes {
	type Attributes = ();

	fn ui(&mut self, ui: &mut Ui, _: (), _: &mut Context) -> bool {
		let mut changed: bool = false;
		let mut debug_mode_data_index: usize = 0_usize;

		for debug_mode_index in 0_usize .. DEBUG_MODE_COUNT {
			let debug_mode: DebugMode = DebugMode::try_from(debug_mode_index).unwrap();
			let mut debug_mode_bool: bool = self.active_debug_modes.get_bit(debug_mode_index);

			if ui.checkbox(&mut debug_mode_bool, debug_mode.as_str()).changed() {
				self.active_debug_modes.set_bit(debug_mode_index, debug_mode_bool);

				if debug_mode_bool {
					self
						.debug_mode_data
						.insert(
							debug_mode_data_index,
							debug_mode.new_data()
						);
				} else {
					self.debug_mode_data.remove(debug_mode_data_index);
				}

				changed = true;
			}

			if debug_mode_bool {
				debug_mode_data_index += 1_usize;
			}
		}

		changed
	}
}

impl PartialEq for DebugModes {
	fn eq(&self, other: &Self) -> bool {
		if self.active_debug_modes == other.active_debug_modes {
			for (
				self_debug_mode_data,
				other_debug_mode_data
			) in self.debug_mode_data.iter().zip(other.debug_mode_data.iter()) {
				if self_debug_mode_data != other_debug_mode_data {
					return false;
				}
			}

			true
		} else {
			false
		}
	}
}