use {
	bevy_inspector_egui::{
		Context,
		Inspectable
	},
	egui::Ui,
	// crate::prelude::*,
	// super::Library as SuperLibrary
};

#[derive(Clone, Default, PartialEq)]
pub struct Library;

impl Inspectable for Library {
	type Attributes = ();

	fn ui(&mut self, _ui: &mut Ui, _: (), _context: &mut Context) -> bool {
		// if ui.button("Dump")

		false
	}
}