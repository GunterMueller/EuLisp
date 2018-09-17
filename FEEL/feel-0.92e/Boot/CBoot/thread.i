((name thread) (dependencies (streams class-names classes bci stream calls extras0 lists threads sems telos1 boot errors init)) (exported-ids ((name . threadp) (address threads threadp) (class . function) (argtype . 1) (position 0)) ((name . set-sig-handler) (address threads set-sig-handler) (class . function) (argtype . 1) (position 1)) ((name . current-thread) (address threads current-thread) (class . function) (argtype . 0) (position 2)) ((name . continuationp) (address threads continuationp) (class . function) (argtype . 1) (position 3)) ((name . feel-machine-type) (address threads feel-machine-type) (class . function) (argtype . 0) (position 4)) ((name . *threads-available*) (address threads *threads-available*) (class . unknown) (argtype . -1) (position 5)) ((name . thread-start) (address threads thread-start) (class . function) (argtype . -2) (position 6)) ((name . thread-set-signalled) (address threads thread-set-signalled) (class . function) (argtype . 2) (position 7)) ((name . internal-thread-reschedule) (address threads internal-thread-reschedule) (class . function) (argtype . 0) (position 8)) ((name . internal-thread-value) (address threads internal-thread-value) (class . function) (argtype . 1) (position 9)) ((name . internal-thread-suspend) (address threads internal-thread-suspend) (class . function) (argtype . 0) (position 10)) ((name . |generic_allocate_instance,Thread_Class|) (address threads |generic_allocate_instance,Thread_Class|) (class . function) (argtype . 2) (position 11)) ((name . initialize-thread) (address threads initialize-thread) (class . function) (argtype . 2) (position 12)) ((name . kick) (address threads kick) (class . function) (argtype . 0) (position 14)) ((name . not-thread-reset) (address threads not-thread-reset) (class . function) (argtype . 1) (position 15)) ((name . *minimum-stack-size*) (address threads *minimum-stack-size*) (class . unknown) (argtype . -1) (position 16)) ((name . thread-state) (address threads thread-state) (class . function) (argtype . 1) (position 17)) ((name . thread-queue) (address threads thread-queue) (class . function) (argtype . 0) (position 18)) ((name . default-thread-stack-size) (address threads default-thread-stack-size) (class . function) (argtype . 0) (position 19)) ((name . |(setter default-thread-stack-size)|) (address threads |(setter default-thread-stack-size)|) (class . function) (argtype . 1) (position 20)) ((name . test-reschedule) (address threads test-reschedule) (class . function) (argtype . 1) (position 21)) ((name . test-gc) (address threads test-gc) (class . function) (argtype . 0) (position 22)) ((name . thread-start) (address threads thread-start) (class . function) (argtype . -2) (position 6)) ((name . current-thread) (address threads current-thread) (class . function) (argtype . 0) (position 2)) ((name . threadp) (address threads threadp) (class . function) (argtype . 1) (position 0)) ((name . <thread>) (address class-names <thread>) (class . unknown) (argtype . -1) (position 15)) ((name . <thread>) (address class-names <thread>) (class . unknown) (argtype . -1) (position 15)) ((address thread make-thread) (name . make-thread) (mutable ()) (class . bytefunction) (argtype (t . 2))) ((address thread thread-signal) (name . thread-signal) (mutable ()) (class . bytefunction) (argtype (() . 3))) ((address thread thread-value) (name . thread-value) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address thread thread-reschedule) (name . thread-reschedule) (mutable ()) (class . bytefunction) (argtype (() . 0))) ((address thread lock-with-signals) (name . lock-with-signals) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address thread <interrupt>) (name . <interrupt>) (mutable ()) (class . unknown)) ((address thread <wrong-thread-continue>) (name . <wrong-thread-continue>) (mutable ()) (class . unknown)) ((address thread <thread-condition>) (name . <thread-condition>) (mutable ()) (class . unknown))))