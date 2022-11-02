use {
    super::{Solver as SolverResource, SolverState, StatType},
    bevy::prelude::*,
    bevy_inspector_egui::{Context, Inspectable},
    egui::{Color32, Ui},
};

#[derive(Clone, Default, PartialEq)]
pub struct Solver;

impl Inspectable for Solver {
    type Attributes = ();

    fn ui(&mut self, ui: &mut Ui, _: (), context: &mut Context) -> bool {
        if let Some(solver) = context
            .world()
            .and_then(World::get_resource::<SolverResource>)
        {
            match solver.solver_state {
                SolverState::Idle => {
                    ui.label("Solver is idle");
                }
                SolverState::Solving(_) => {
                    solver.stats.type_stats_array_ui(ui);
                    solver.stats.full_mask_and_completion.immut_ui(ui);
                }
                SolverState::Solved | SolverState::DidNotFinish => {
                    if solver.is_solved() {
                        ui.label("Solved");
                    } else {
                        ui.colored_label(Color32::RED, "Did Not Finish");
                    }

                    solver.stats.type_stats_array[StatType::Total as usize].immut_ui(ui);
                }
            }
        } else {
            ui.colored_label(
                Color32::RED,
                format!(
                    "Missing {}",
                    if context.world().is_none() {
                        "World"
                    } else {
                        "solver::Solver resource"
                    }
                ),
            );
        }

        false
    }
}
