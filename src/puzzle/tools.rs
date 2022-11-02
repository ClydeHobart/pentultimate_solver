use {
    super::{
        transformation::{library::FamilyInput, HalfAddrAttrs},
        HalfAddr, InflatedPieceStateComponent as PSC,
    },
    crate::{
        app::prelude::*,
        piece::consts::*,
        prelude::*,
        ui::input::{PendingActions, PuzzleAction, PuzzleActionType},
        util::{inspectable_bit_array::InspectableBitArray, inspectable_num::InspectableNum},
    },
    bevy::prelude::*,
    bevy_inspector_egui::{options::NumberAttributes, Context, Inspectable},
    egui::{Button, Color32, Grid, Separator, TextEdit, Ui},
    std::ops::Range,
};

#[derive(Clone, Default, Inspectable, PartialEq)]
pub struct StatsOptions {
    reorientation: HalfAddr,
    #[inspectable(collapse, length = usize::PENTAGON_VERTEX_COUNT)]
    desired_pent_rot_sums: InspectableBitArray<u8>,
    #[inspectable(collapse, length = usize::TRIANGLE_VERTEX_COUNT)]
    desired_tri_rot_sums: InspectableBitArray<u8>,
}

#[derive(Clone, Default, PartialEq)]
pub struct PuzzleState {
    stats_options: StatsOptions,
}

impl Inspectable for PuzzleState {
    type Attributes = ();

    fn ui(&mut self, ui: &mut Ui, _: (), context: &mut Context) -> bool {
        if context.world().is_none() {
            return false;
        }

        context.world_scope(
            ui,
            "",
            |world: &mut World, ui: &mut Ui, context: &mut Context| -> bool {
                if let Some(extended_puzzle_state) = world.get_resource::<ExtendedPuzzleState>() {
                    let mut changed: bool = false;

                    ui.collapsing("Position & Rotation", |ui: &mut Ui| -> () {
                        ui.monospace(format!("{:#?}", extended_puzzle_state.puzzle_state));
                    });
                    ui.collapsing("Stats", |ui: &mut Ui| -> () {
                        changed |= self.stats_options.ui(ui, (), context);

                        Grid::new("StatsTable").show(ui, |ui: &mut Ui| -> () {
                            let InflatedPuzzleState { pos, rot } =
                                if self.stats_options.reorientation.is_valid() {
                                    &extended_puzzle_state.puzzle_state
                                        + self.stats_options.reorientation.as_reorientation()
                                } else {
                                    extended_puzzle_state.puzzle_state.clone()
                                };

                            let mut correct_pent_pos_count: u32 = 0_u32;
                            let mut correct_tri_pos_count: u32 = 0_u32;
                            let mut correct_pent_rot_count: u32 = 0_u32;
                            let mut correct_tri_rot_count: u32 = 0_u32;
                            let mut pent_rot_sum: PSC = ZERO_IPSC;
                            let mut tri_rot_sum: PSC = ZERO_IPSC;

                            for pent_index in PENTAGON_PIECE_RANGE {
                                correct_pent_pos_count +=
                                    (pos[pent_index] as usize == pent_index) as u32;
                                correct_pent_rot_count += (rot[pent_index] == ZERO_IPSC) as u32;
                                pent_rot_sum += rot[pent_index];
                            }

                            pent_rot_sum %= PSC::PENTAGON_VERTEX_COUNT;

                            for tri_index in TRIANGLE_PIECE_RANGE {
                                correct_tri_pos_count +=
                                    (pos[tri_index] as usize == tri_index) as u32;
                                correct_tri_rot_count += (rot[tri_index] == ZERO_IPSC) as u32;
                                tri_rot_sum += rot[tri_index];
                            }

                            tri_rot_sum %= PSC::TRIANGLE_VERTEX_COUNT;

                            let desired_pent_rot_sum: bool =
                                self.stats_options.desired_pent_rot_sums.0[pent_rot_sum as usize];
                            let desired_tri_rot_sum: bool =
                                self.stats_options.desired_tri_rot_sums.0[tri_rot_sum as usize];
                            let desired_rot_sum: bool = desired_pent_rot_sum && desired_tri_rot_sum;

                            macro_rules! colored_label {
                                ($count:expr, $max:expr) => {
                                    let count: PSC = $count;

                                    ui.colored_label(
                                        Color32::from_alt(red_to_green(count as f32 / $max)),
                                        format!("{}", count),
                                    );
                                };
                            }

                            ui.label("");
                            ui.label("Pents").on_hover_text("Pentagon Pieces");
                            ui.label("Tris").on_hover_text("Triangle Pieces");
                            ui.label("Total").on_hover_text("All Pieces");
                            ui.end_row();

                            ui.label("Correct Pos")
                                .on_hover_text("Correct Position Count");
                            colored_label!(correct_pent_pos_count, f32::PENTAGON_PIECE_COUNT);
                            colored_label!(correct_tri_pos_count, f32::TRIANGLE_PIECE_COUNT);
                            colored_label!(
                                correct_pent_pos_count + correct_tri_pos_count,
                                f32::PIECE_COUNT
                            );
                            ui.end_row();

                            ui.label("Correct Rot")
                                .on_hover_text("Correct Rotation Count");
                            colored_label!(correct_pent_rot_count, f32::PENTAGON_PIECE_COUNT);
                            colored_label!(correct_tri_rot_count, f32::TRIANGLE_PIECE_COUNT);
                            colored_label!(
                                correct_pent_rot_count + correct_tri_rot_count,
                                f32::PIECE_COUNT
                            );
                            ui.end_row();

                            ui.label("Rot Sum")
                                .on_hover_text("Rotation Sum (% Piece Side Count)");
                            ui.colored_label(
                                Color32::from_alt(red_to_green(desired_pent_rot_sum as u8 as f32)),
                                format!("{}", pent_rot_sum),
                            );
                            ui.colored_label(
                                Color32::from_alt(red_to_green(desired_tri_rot_sum as u8 as f32)),
                                format!("{}", tri_rot_sum),
                            );
                            ui.colored_label(
                                Color32::from_alt(red_to_green(desired_rot_sum as u8 as f32)),
                                format!("{}", if desired_rot_sum { "✔" } else { "❌" }),
                            );
                            ui.end_row();
                        });
                    });

                    changed
                } else {
                    false
                }
            },
        )
    }
}

#[derive(Clone, Default, PartialEq)]
pub struct Stack {
    new_family_name: String,
    pub min_focus_index: usize,
    pub max_focus_index: usize,
    debug_addr: HalfAddr,
    pub print_true_action: bool,
}

impl Stack {
    fn focus_range(&self) -> Range<usize> {
        self.min_focus_index..self.max_focus_index + 1_usize
    }
}

impl Inspectable for Stack {
    type Attributes = ();

    fn ui(&mut self, ui: &mut Ui, _: (), context: &mut Context) -> bool {
        let mut changed: bool = false;

        ui.horizontal(|ui: &mut Ui| -> () {
            ui.label("New Family Name");

            changed |= TextEdit::singleline(&mut self.new_family_name)
                .show(ui)
                .response
                .changed();
        });
        ui.collapsing("Focus Indices", |ui: &mut Ui| -> () {
            Grid::new(context.id())
                .min_col_width(0.0_f32)
                .show(ui, |ui: &mut Ui| -> () {
                    ui.label("Min");
                    ui.add(Separator::default().vertical());
                    changed = <&mut InspectableNum<usize>>::from(&mut self.min_focus_index).ui(
                        ui,
                        NumberAttributes::<usize>::between(0_usize, self.max_focus_index),
                        &mut context.with_id(0_u64),
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
                                    world.get_resource::<ExtendedPuzzleState>().map(
                                        |extended_puzzle_state: &ExtendedPuzzleState| -> usize {
                                            extended_puzzle_state.actions.len().max(1_usize)
                                                - 1_usize
                                        },
                                    )
                                })
                                .unwrap_or(max_focus_index),
                        ),
                        &mut context.with_id(1_u64),
                    );
                    ui.end_row();
                });
        });
        ui.collapsing("Debug Addr", |ui: &mut Ui| -> () {
            self.debug_addr
                .ui(ui, HalfAddrAttrs::default(), &mut context.with_id(2_u64));
        });
        ui.horizontal(|ui: &mut Ui| -> () {
            ui.label("Print True Action");
            self.print_true_action
                .ui(ui, (), &mut context.with_id(3_u64));
        });
        ui.separator();

        let mut world: Option<&mut World> = unsafe { context.world_mut() };
        let focus_range: Range<usize> = self.focus_range();
        let (can_simplify_actions, can_add_family, can_set_camera_start, can_reorient_actions):
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
                        extended_puzzle_state.actions_are_simplified(&focus_range)
                            && !self.new_family_name.is_empty()
                            && GenusIndex::try_from(self.new_family_name.as_str()).is_err(),
                        debug_addr_is_valid,
                        debug_addr_is_valid && extended_puzzle_state.can_reorient_actions(&focus_range)
                    )
                })
                .map(|
                    (can_simplify_actions, can_add_family, can_set_camera_start, can_reorient_actions):
                    (bool, bool, bool, bool)
                | -> (bool, bool, bool, bool) {
                    (
                        can_simplify_actions,
                        can_add_family,
                        can_set_camera_start,
                        can_reorient_actions && {
                            /* this part needs to be in its own map() call because immutable resources are still in
                            scope during the first call */
                            let world: &mut World = world.as_mut().unwrap();

                            world.contains_resource::<Preferences>()
                                && world.query::<CameraComponents>().iter(world).next().is_some()
                        }
                    )
                })
                .unwrap_or_default();

        if ui
            .add_enabled(can_simplify_actions, Button::new("Simplify Actions"))
            .clicked()
        {
            world
                .as_mut()
                .unwrap()
                .get_resource_mut::<ExtendedPuzzleState>()
                .unwrap()
                .simplify_actions(&focus_range);
            changed = true;
        }

        if ui
            .add_enabled(can_add_family, Button::new("Add Actions As Family"))
            .clicked()
        {
            warn_expect_some!(
                world
                    .as_ref()
                    .unwrap()
                    .get_resource::<ExtendedPuzzleState>()
                    .unwrap()
                    .get_as_seed_simples(&focus_range),
                |seed_simples: Vec<HalfAddr>| -> () {
                    warn_expect_ok!(TransformationLibrary::push_family_and_update_file(
                        FamilyInput {
                            name: self.new_family_name.clone(),
                            seed_simples: seed_simples
                                .iter()
                                .map(|simple: &HalfAddr| -> (u8, u8) {
                                    (
                                        simple.get_species_index() as u8,
                                        simple.get_organism_index() as u8,
                                    )
                                })
                                .collect()
                        }
                    ));
                }
            );
        }

        if ui
            .add_enabled(can_set_camera_start, Button::new("Set Camera Start"))
            .clicked()
        {
            world
                .as_mut()
                .unwrap()
                .get_resource_mut::<ExtendedPuzzleState>()
                .unwrap()
                .set_camera_start(&focus_range, self.debug_addr);
            changed = true;
        }

        if ui
            .add_enabled(
                can_set_camera_start,
                Button::new("Set Initial Camera Start"),
            )
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

        if ui
            .add_enabled(can_reorient_actions, Button::new("Reorient Actions"))
            .clicked()
        {
            let puzzle_action: Option<PuzzleAction> = {
                let world: &mut World = world.as_mut().unwrap();

                Some(PuzzleAction {
                    current_action: None,
                    pending_actions: Some(Box::new({
                        let mut extended_puzzle_state: ExtendedPuzzleState =
                            world.get_resource::<ExtendedPuzzleState>().unwrap().clone();
                        let mut camera: HalfAddr =
                            CameraQueryStateNT(&mut (world.query::<CameraComponents>()))
                                .orientation(
                                    world,
                                    |camera_orientation: Option<&Quat>| -> HalfAddr {
                                        camera_orientation.map_or(
                                            HalfAddr::default(),
                                            CameraPlugin::compute_camera_addr,
                                        )
                                    },
                                );

                        extended_puzzle_state.reorient_actions(
                            &focus_range,
                            self.debug_addr,
                            &mut camera,
                        );

                        PendingActions::load(
                            &SaveState {
                                extended_puzzle_state,
                                input_toggles: world
                                    .get_resource::<InputState>()
                                    .unwrap()
                                    .toggles
                                    .clone(),
                                camera,
                            },
                            &world.get_resource::<Preferences>().unwrap().speed.animation,
                        )
                    })),
                    action_type: PuzzleActionType::Load,
                })
            };

            world
                .as_mut()
                .unwrap()
                .get_resource_mut::<InputState>()
                .unwrap()
                .puzzle_action = puzzle_action;
        }

        changed
    }
}
