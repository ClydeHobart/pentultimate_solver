use {
    bevy_inspector_egui::{Context, Inspectable},
    egui::Ui,
};

#[derive(Clone, Default, PartialEq)]
pub struct Library;

impl Inspectable for Library {
    type Attributes = ();

    fn ui(&mut self, _ui: &mut Ui, _: (), _context: &mut Context) -> bool {
        false
    }
}
