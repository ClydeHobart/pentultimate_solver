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
		},
		ops::Range
	}
};

pub trait InstpectableBitArraySuperTrait:
		BitField
		+ Clone
		+ Default
		+ PartialEq
{
	fn field_index_range(length: usize) -> Range<usize> {
		0_usize .. (length + Self::BIT_LENGTH - 1_usize) / Self::BIT_LENGTH
	}

	fn needs_mask(field_index: usize, length: usize) -> Option<usize> {
		let remaining_bits: usize = length - field_index * Self::BIT_LENGTH;

		if remaining_bits != 0_usize && remaining_bits < Self::BIT_LENGTH {
			Some(remaining_bits)
		} else {
			None
		}
	}

	#[allow(unused_variables)]
	fn set_all(bit_array: &mut [Self], length: usize) -> () {}
	#[allow(unused_variables)]
	fn clear_all(bit_array: &mut [Self], length: usize) -> () {}
	#[allow(unused_variables)]
	fn toggle_all(bit_array: &mut [Self], length: usize) -> () {}
}

macro_rules! impl_inspectable_bit_array_super_trait {
	($($type:ty),*) => {
		$(
			impl InstpectableBitArraySuperTrait for $type {
				fn set_all(bit_array: &mut [Self], length: usize) -> () {
					for field_index in Self::field_index_range(length) {
						if let Some(remaining_bits) = Self::needs_mask(field_index, length) {
							bit_array[field_index] |= ((1 as $type << remaining_bits) - 1 as $type);
						} else {
							bit_array[field_index] = <$type>::MAX;
						}
					}
				}

				fn clear_all(bit_array: &mut [Self], length: usize) -> () {
					for field_index in Self::field_index_range(length) {
						if let Some(remaining_bits) = Self::needs_mask(field_index, length) {
							bit_array[field_index] &= !((1 as $type << remaining_bits) - 1 as $type);
						} else {
							bit_array[field_index] = 0 as $type;
						}
					}
				}

				fn toggle_all(bit_array: &mut [Self], length: usize) -> () {
					for field_index in Self::field_index_range(length) {
						if let Some(remaining_bits) = Self::needs_mask(field_index, length) {
							bit_array[field_index] ^= ((1 as $type << remaining_bits) - 1 as $type);
						} else {
							bit_array[field_index] ^= <$type>::MAX;
						}
					}
				}
			}
		)*
	};
}

impl_inspectable_bit_array_super_trait!(u8, u16, u32, u64, u128);

#[derive(Clone, PartialEq)]
pub struct InspectableBitArray<T: InstpectableBitArraySuperTrait, const N: usize>(pub [T; N]);

impl<T: InstpectableBitArraySuperTrait, const N: usize> Default for InspectableBitArray<T, N> {
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

impl<T: InstpectableBitArraySuperTrait, const N: usize> Inspectable for InspectableBitArray<T, N> {
	type Attributes = InspectableBitArrayAttrs;

	fn ui(&mut self, ui: &mut Ui, options: Self::Attributes, _: &Context) -> bool {
		let length: usize = min(
			options.length.map_or(usize::MAX, |length: usize| -> usize { length }),
			self.0.bit_length()
		);
		let mut changed: bool = false;

		ui.horizontal(|ui: &mut Ui| -> () {
			if ui.small_button("| 1").clicked() {
				T::set_all(&mut self.0, length);
			}

			if ui.small_button("& 0").clicked() {
				T::clear_all(&mut self.0, length);
			}

			if ui.small_button("^ 1").clicked() {
				T::toggle_all(&mut self.0, length);
			}
		});

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