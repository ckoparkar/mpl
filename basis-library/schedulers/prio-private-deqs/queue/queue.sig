signature QUEUE =
sig

    type task
    type t
    type task_set

    val empty : unit -> t
    (* Create a new queue from a task set. *)
    val fromSet : task_set -> t

    (* Push a task *in order* to the bottom of the queue *)
    val push : t -> task -> t
    (* Insert an arbitrary task to its correct position in the queue *)
    val insert : t -> task -> t

    (* Pull from the "bottom" of the queue. Return NONE if the queue
     * is empty. *)
    val choose : t -> (task option) * t
    (* Returns a set of tasks consisting of at least 1/4 of the total potential,
     * and, if the set of tasks has size > 1, at most 3/4 of the total.
     * Returns NONE if the queue is empty or has one element. *)
    val split : t -> (task_set option) * t

end
