use self::{
	data::*,
	uses::*
};

pub mod prelude {
	pub use super::{
		data::{
			ToolDataBox,
			Stack
		},
		Tool
	};
}

mod uses {
	pub use {
		std::{
			any::Any,
			convert::TryFrom,
			mem::transmute,
			ops::{
				Deref,
				Range
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
		crate::{
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
		}
	};
}

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
	Solver,
	Stack
);

const TOOL_COUNT: usize = tool_count();

const_assert!(TOOL_COUNT <= ToolInner::MAX as usize);

mod data {
	pub use crate::puzzle::{
		solver::tools::Solver,
		tools::{
			PuzzleState,
			Stack
		}
	};

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