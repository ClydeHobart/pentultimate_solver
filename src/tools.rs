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
		bitvec::{
			array::BitArray,
			BitArr
		},
		egui::{
			widgets::Separator,
			Button,
			CollapsingHeader,
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
			preferences::Update,
			prelude::*,
			piece::consts::*,
			puzzle::{
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

		impl Tool {
			const COUNT: usize = Self::count();
			const ALL_TOOLS: [Self; Self::COUNT] = [
				$(
					Self::$tool,
				)*
			];
			const ALL_STRS: [&'static str; Self::COUNT] = [
				$(
					stringify!($tool),
				)*
			];

			fn as_str(self) -> &'static str { Self::ALL_STRS[self as usize] }

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

			const fn count() -> usize {
				#![allow(path_statements)]
	
				let mut count: usize = 0_usize;
	
				$(
					Tool::$tool;
					count += 1_usize;
				)*
	
				count
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

const_assert!(Tool::COUNT <= ToolInner::MAX as usize);

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
		if value < Self::COUNT {
			Ok(unsafe { transmute::<ToolInner, Self>(value as ToolInner) })
		} else {
			// Return how much out of range the value is
			Err(value - Self::COUNT + 1_usize)
		}
	}
}

type ToolsBitArray = BitArr!(for Tool::COUNT, in u32);

#[derive(Clone)]
pub struct ToolsData {
	active_tools:		ToolsBitArray,
	tool_data_boxes:	Vec<ToolDataBox>
}

impl ToolsData {
	pub fn all() -> Self { Tool::ALL_TOOLS.iter().into() }

	pub fn none() -> Self {
		Self {
			active_tools:		ToolsBitArray::default(),
			tool_data_boxes:	Vec::<ToolDataBox>::new()
		}
	}

	pub fn should_render(&self) -> bool { !self.tool_data_boxes.is_empty() }

	pub fn is_tool_active(&self, tool: Tool) -> bool { self.active_tools[tool as usize] }

	pub fn get_tool_data(&self, tool: Tool) -> Option<&ToolDataBox> {
		if self.is_tool_active(tool) {
			Some(&self.tool_data_boxes[self.active_tools[0_usize .. tool as usize].count_ones()])
		} else {
			None
		}
	}

	pub fn render(&mut self, ui: &mut Ui, context: &mut Context) -> () {
		CollapsingHeader::new("Tools")
			.default_open(true)
			.show(ui, |ui: &mut Ui| -> () {
				let mut tool_data_index: usize = 0_usize;

				for tool_index in 0_usize .. Tool::COUNT {
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

impl Default for ToolsData {
	fn default() -> Self { Self::none() }
}

impl<'de> Deserialize<'de> for ToolsData {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		Ok(Vec::<Tool>::deserialize(deserializer)?.iter().into())
	}
}

impl<'a, T: Iterator<Item = &'a Tool>> From<T> for ToolsData {
	fn from(tool_iter: T) -> Self {
		let mut tools_data: Self = Self::none();

		for tool in tool_iter {
			tools_data.active_tools.set(*tool as usize, true);
		}

		for tool_index in 0_usize .. Tool::COUNT {
			if tools_data.active_tools[tool_index] {
				tools_data.tool_data_boxes.push(Tool::try_from(tool_index).unwrap().new_data());
			}
		}

		tools_data
	}
}

impl Inspectable for ToolsData {
	type Attributes = ();

	fn ui(&mut self, ui: &mut Ui, _: (), _: &mut Context) -> bool {
		let mut changed: bool = false;
		let mut tool_data_index: usize = 0_usize;

		for tool_index in 0_usize .. Tool::COUNT {
			let tool: Tool = Tool::try_from(tool_index).unwrap();
			let mut tool_bool: bool = self.active_tools[tool_index];

			if ui.checkbox(&mut tool_bool, tool.as_str()).changed() {
				self.active_tools.set(tool_index, tool_bool);

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