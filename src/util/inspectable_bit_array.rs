use {
    bevy_inspector_egui::{Context, Inspectable},
    bitvec::{prelude::*, view::BitViewSized},
    egui::Ui,
    std::{cmp::min, rc::Rc},
};

#[derive(Clone, Default)]
pub struct InspectableBitSliceAttrs {
    pub length: Option<usize>,
    pub fetch_label: Option<Rc<dyn Fn(usize) -> String>>,
}

#[derive(Clone, Default, PartialEq)]
pub struct InspectableBitArray<A: BitViewSized = [usize; 1_usize], O: BitOrder = Lsb0>(
    pub BitArray<A, O>,
);

impl<A: BitViewSized, O: BitOrder> Inspectable for InspectableBitArray<A, O> {
    type Attributes = InspectableBitSliceAttrs;

    fn ui(&mut self, ui: &mut Ui, options: Self::Attributes, context: &mut Context) -> bool {
        let mut inspectable_bit_slice: InspectableBitSlice<A::Store, O> =
            InspectableBitSlice::<A::Store, O>(&mut self.0);

        inspectable_bit_slice.ui(ui, options, context)
    }
}

pub struct InspectableBitSlice<'a, T: BitStore, O: BitOrder>(pub &'a mut BitSlice<T, O>);

impl<'a, T: BitStore, O: BitOrder> Inspectable for InspectableBitSlice<'a, T, O> {
    type Attributes = InspectableBitSliceAttrs;

    fn ui(&mut self, ui: &mut Ui, options: Self::Attributes, _: &mut Context) -> bool {
        let length: usize = min(options.length.unwrap_or(usize::MAX), self.0.len());
        let mut changed: bool = false;

        ui.vertical(|ui: &mut Ui| {
            ui.horizontal(|ui: &mut Ui| {
                if ui.small_button("| 1").clicked() {
                    self.0[0_usize..length].fill(true);
                }

                if ui.small_button("& 0").clicked() {
                    self.0[0_usize..length].fill(false);
                }

                if ui.small_button("^ 1").clicked() {
                    (!&mut self.0[0_usize..length]).len();
                }
            });

            for bit in 0_usize..length {
                let mut is_checked: bool = self.0[bit];

                if ui
                    .checkbox(
                        &mut is_checked,
                        options.fetch_label.as_ref().map_or_else(
                            || -> String { format!("{}", bit) },
                            |fetch_label: &Rc<dyn Fn(usize) -> String>| -> String {
                                (fetch_label)(bit)
                            },
                        ),
                    )
                    .changed()
                {
                    self.0.set(bit, is_checked);
                    changed = true;
                }
            }
        });

        changed
    }
}
