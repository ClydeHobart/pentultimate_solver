use {
	crate::define_super_trait,
	bevy_inspector_egui::{
		options::NumberAttributes,
		Context,
		Inspectable
	},
	egui::Ui,
	num_traits::{
		Bounded,
		One
	},
	std::ops::{
		AddAssign,
		SubAssign
	}
};

define_super_trait!(pub trait InspectableNumTrait:
	AddAssign,
	Bounded,
	Clone,
	Copy,
	Default,
	Inspectable<Attributes = NumberAttributes<Self>>,
	PartialEq,
	PartialOrd,
	One,
	SubAssign
);

#[derive(Clone, Default, PartialEq)]
pub struct InspectableNum<T: InspectableNumTrait>(pub T);

impl<T: InspectableNumTrait> Inspectable for InspectableNum<T> {
type Attributes = NumberAttributes<T>;

fn ui(&mut self, ui: &mut Ui, options: NumberAttributes<T>, context: &mut Context) -> bool {
	let options: NumberAttributes<T> = NumberAttributes::<T> {
		min: options.min.or(Some(T::min_value())),
		max: options.max.or(Some(T::max_value())),
		.. options
	};

	let mut changed: bool = false;

	ui.scope(|ui: &mut Ui| -> () {
		ui.set_enabled(self.0 > *options.min.as_ref().unwrap());

		if ui.small_button("min").clicked() {
			self.0 = *options.min.as_ref().unwrap();
			changed = true;
		}
	});
	ui.scope(|ui: &mut Ui| -> () {
		ui.set_enabled(self.0 > *options.min.as_ref().unwrap());

		if ui.small_button("-").clicked() {
			self.0 -= T::one();
			changed = true;
		}
	});
	changed |= self.0.ui(ui, options.clone(), context);
	ui.scope(|ui: &mut Ui| -> () {
		ui.set_enabled(self.0 < *options.max.as_ref().unwrap());

		if ui.small_button("+").clicked() {
			self.0 += T::one();
			changed = true;
		}
	});
	ui.scope(|ui: &mut Ui| -> () {
		ui.set_enabled(self.0 < *options.max.as_ref().unwrap());

		if ui.small_button("max").clicked() {
			self.0 = *options.max.as_ref().unwrap();
			changed = true;
		}
	});

	changed
}
}