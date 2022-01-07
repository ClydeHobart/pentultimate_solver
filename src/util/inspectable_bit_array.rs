use {
	bevy_inspector_egui::{
		Context,
		Inspectable
	},
	bit_field::{
		BitField,
		BitArray
	},
	egui::Ui,
	std::{
		cmp::min,
		mem::{
			MaybeUninit,
			transmute_copy
		}
	}
};

#[derive(Clone, PartialEq)]
pub struct InspectableBitArray<T: BitField + Clone + Default + PartialEq, const N: usize>(pub [T; N]);

impl<T: BitField + Clone + Default + PartialEq, const N: usize> Default for InspectableBitArray<T, N> {
	fn default() -> Self { unsafe { MaybeUninit::<Self>::zeroed().assume_init() } }
}

#[derive(Default)]
pub struct InspectableBitArrayAttrs {
	pub length:			Option<usize>,
	pub fetch_label:	Option<Box<dyn Fn(usize) -> &'static str>>
}

impl Clone for InspectableBitArrayAttrs {
	fn clone(&self) -> Self { unsafe { transmute_copy::<Self, Self>(self) } }
}

impl<T: BitField + Clone + Default + PartialEq, const N: usize> Inspectable for InspectableBitArray<T, N> {
	type Attributes = InspectableBitArrayAttrs;

	fn ui(&mut self, ui: &mut Ui, options: Self::Attributes, _: &Context) -> bool {
		let length: usize = min(
			options.length.map_or(usize::MAX, |length: usize| -> usize { length }),
			self.0.bit_length()
		);
		let mut changed: bool = false;

		for bit in 0_usize .. length {
			let mut is_checked: bool = self.0.get_bit(bit);

			if ui.checkbox(
				&mut is_checked,
				options.fetch_label.as_ref().map_or_else(
					|| -> String { format!("{}", bit) },
					|fetch_label: &Box<dyn Fn(usize) -> &'static str>| -> String { (fetch_label)(bit).into() }
				)
			).changed() {
				self.0.set_bit(bit, is_checked);
				changed = true;
			}
		}

		changed
	}
}