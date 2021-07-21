//! Contains only the logic related to how the symbolic execution engine handles
//! the graph-theory part of symbolic execution. This allows the symbolic
//! execution engine to focus on handling symbolic execution, while behavior
//! regarding cyclic executions are handled by the graph system.

use std::{
    hash::Hash,
    sync::{Arc, Mutex},
};

use rustc_hash::FxHashMap;

/// Implemented by nodes that are a part of the graph system. Provides an
/// interface for making requests to the graph system.
pub trait Worker {
    /// The unique ID of the worker, used in order for workers to locate other
    /// workers.
    type Id: Copy + Eq + Hash;

    /// The type of results produced by a worker node, which are consumed by
    /// other worker nodes.
    type Result: Bogusable;

    /// Performs the work required of this worker node. The node is given a
    /// reference to a system to use to make requests to other worker nodes.
    fn work(self, system: &impl System<Self>) -> Self::Result;
}

/// Contract that a system provides. This is used for the [`Worker::work`]
/// method so that workers can call upon the system to invoke other workers.
pub trait System<W>
where
    W: ?Sized + Worker,
{
    /// Given the ID of a worker, this function will execute that worker and
    /// produce the value that the worker executed.
    fn spawn(&self, id: W::Id) -> Arc<W::Result>;
}

/// A trait implemented on something that is capable of returning a bogus value.
/// When a system enters a cyclic state, no worker is capable of making progress
/// as it depends upon the other workers to make progress. When this state is
/// reached, a bogus value is produced by the system. This bogus value can then
/// work through the system until a result is produced. In addition, the system
/// may re-execute nodes in order to refine the value produced.
// TODO: remove Bogusable in favor of having `System::spawn`
// return an `Option<Arc<Result>>`
pub trait Bogusable {
    /// Produces the a bogus value.
    fn bogus() -> Self;
}

/// Given an ID, will produce a worker.
pub trait WorkerFactory {
    type Worker: Worker;

    fn make(&mut self, id: <Self::Worker as Worker>::Id) -> Self::Worker;
}

#[derive(Clone)]
pub struct GraphSystem<W: Worker, F>(Arc<GraphSystemInner<W, W::Id, F>>);

#[derive(Debug)]
pub enum ResultsError {
    ReferencesToSystem,
    LockHeldOnWorkers,
    ReferencesToWorkerResults,
}

impl<W, F> GraphSystem<W, F>
where
    W: Worker,
    F: WorkerFactory<Worker = W>,
{
    pub fn new(factory: F) -> Self {
        GraphSystem(Arc::new(GraphSystemInner {
            workers: Mutex::default(),
            factory: Mutex::new(factory),
        }))
    }

    pub fn try_into_results(self) -> Result<FxHashMap<W::Id, W::Result>, ResultsError> {
        let system_inner = Arc::try_unwrap(self.0).map_err(|_| ResultsError::ReferencesToSystem)?;
        let workers =
            Mutex::into_inner(system_inner.workers).map_err(|_| ResultsError::LockHeldOnWorkers)?;

        let mut results = FxHashMap::default();
        for (id, state) in workers {
            let result = match state.status {
                WorkStatus::Working => {
                    panic!("system should be in finished state, yet workers remain working")
                }
                WorkStatus::Completed(r) => r,
            };

            let result =
                Arc::try_unwrap(result).map_err(|_| ResultsError::ReferencesToWorkerResults)?;
            results.insert(id, result);
        }
        Ok(results)
    }
}

struct GraphSystemInner<W: Worker, I, F> {
    workers: Mutex<FxHashMap<I, GraphWorkerState<Arc<W::Result>>>>,
    factory: Mutex<F>,
}

enum WorkStatus<R> {
    Working,
    Completed(R),
}

struct GraphWorkerState<R> {
    status: WorkStatus<R>,
}

impl<W, F> System<W> for GraphSystem<W, F>
where
    W: Worker,
    F: WorkerFactory<Worker = W>,
{
    fn spawn(&self, id: W::Id) -> Arc<W::Result> {
        GraphSystemInner::spawn(&self.0, id)
    }
}

impl<W, F> GraphSystemInner<W, W::Id, F>
where
    W: Worker,
    F: WorkerFactory<Worker = W>,
{
    fn spawn(me: &Arc<GraphSystemInner<W, W::Id, F>>, id: W::Id) -> Arc<W::Result> {
        let mut workers = me.workers.try_lock().expect("should be contentionless");

        // check if we've already executed this worker
        if let Some(worker) = workers.get(&id) {
            match &worker.status {
                WorkStatus::Working => panic!("cyclic workers not supported yet"),
                WorkStatus::Completed(r) => return r.clone(),
            };
        }

        // we've never executed a worker with this id
        // produce a new one
        let mut factory = me.factory.try_lock().expect("should be contentionless");
        let worker = factory.make(id);
        drop(factory);

        // record this worker as working
        let state = GraphWorkerState {
            status: WorkStatus::Working,
        };
        workers.insert(id, state);

        // drop the lock so that the `work` call won't deadlock
        drop(workers);

        // perform the work
        let sys_api = GraphSystem(me.clone());
        let result = Arc::new(worker.work(&sys_api));

        // update the worker status
        let mut workers = me.workers.try_lock().expect("should be contentionless");
        let state = workers.get_mut(&id).unwrap();
        state.status = WorkStatus::Completed(result.clone());

        result
    }
}
