1856
((3) 0 () 1 ((q lib "neuron/process.rkt")) () (h ! (equal) ((c def c (c (? . 0) q command)) q (329 . 4)) ((c def c (c (? . 0) q shutdown)) q (3320 . 3)) ((c def c (c (? . 0) q process?)) q (233 . 3)) ((c def c (c (? . 0) q take-evt)) q (1118 . 2)) ((c def c (c (? . 0) q deadlock)) q (773 . 3)) ((c def c (c (? . 0) q pipe)) q (2532 . 4)) ((c def c (c (? . 0) q alive?)) q (664 . 3)) ((c def c (c (? . 0) q emit)) q (1149 . 3)) ((c def c (c (? . 0) q give)) q (884 . 4)) ((c def c (c (? . 0) q shutdown-evt)) q (3376 . 3)) ((c def c (c (? . 0) q all-evts)) q (1469 . 3)) ((c def c (c (? . 0) q socket)) q (2390 . 5)) ((c form c (c (? . 0) q while)) q (1405 . 2)) ((c form c (c (? . 0) q until)) q (1437 . 2)) ((c def c (c (? . 0) q serve)) q (1692 . 7)) ((c def c (c (? . 0) q source)) q (2159 . 7)) ((c def c (c (? . 0) q current-process)) q (287 . 2)) ((c def c (c (? . 0) q dead-evt)) q (829 . 3)) ((c def c (c (? . 0) q try-take)) q (1086 . 2)) ((c def c (c (? . 0) q quit)) q (401 . 3)) ((c def c (c (? . 0) q give-evt)) q (971 . 4)) ((c def c (c (? . 0) q die)) q (453 . 3)) ((c def c (c (? . 0) q seq-evts)) q (1527 . 3)) ((c def c (c (? . 0) q recv-evt)) q (1322 . 3)) ((c def c (c (? . 0) q dead?)) q (608 . 3)) ((c def c (c (? . 0) q take)) q (1058 . 2)) ((c def c (c (? . 0) q process)) q (0 . 7)) ((c def c (c (? . 0) q proxy)) q (2791 . 6)) ((c def c (c (? . 0) q kill)) q (556 . 3)) ((c form c (c (? . 0) q forever)) q (1377 . 2)) ((c def c (c (? . 0) q sink)) q (1927 . 7)) ((c def c (c (? . 0) q managed)) q (3014 . 9)) ((c def c (c (? . 0) q loop-evts)) q (1609 . 3)) ((c def c (c (? . 0) q msg/c)) q (3435 . 2)) ((c def c (c (? . 0) q emit-evt)) q (1209 . 3)) ((c def c (c (? . 0) q recv)) q (1272 . 3)) ((c def c (c (? . 0) q bridge)) q (2649 . 5)) ((c def c (c (? . 0) q wait)) q (721 . 3)) ((c def c (c (? . 0) q stop)) q (504 . 3))))
procedure
(process  thunk                   
         [#:on-stop on-stop       
          #:command handler]) -> process?
  thunk : (-> any)
  on-stop : (-> any) = void
  handler : (-> any) = void
procedure
(process? v) -> boolean?
  v : any/c
procedure
(current-process) -> process?
procedure
(command π v) -> any
  π : process?
  v : any/c
procedure
(quit v ...) -> void?
  v : any/c
procedure
(die v ...) -> void?
  v : any/c
procedure
(stop π) -> void?
  π : process?
procedure
(kill π) -> void?
  π : process?
procedure
(dead? π) -> boolean?
  π : process?
procedure
(alive? π) -> boolean?
  π : process?
procedure
(wait π) -> void?
  π : process?
procedure
(deadlock v ...) -> void?
  v : any/c
procedure
(dead-evt π) -> evt?
  π : process?
procedure
(give π [v]) -> boolean?
  π : process?
  v : msg/c = (void)
procedure
(give-evt π [v]) -> evt?
  π : process?
  v : msg/c = (void)
procedure
(take) -> any/c
procedure
(try-take) -> any/c
procedure
(take-evt) -> evt?
procedure
(emit [v]) -> void?
  v : msg/c = (void)
procedure
(emit-evt [v]) -> evt?
  v : msg/c = (void)
procedure
(recv π) -> any
  π : process?
procedure
(recv-evt π) -> evt?
  π : process?
syntax
(forever body ...)
syntax
(while expr body ...)
syntax
(until expr body ...)
procedure
(all-evts evt ...) -> evt?
  evt : evt?
procedure
(seq-evts make-evt ...+) -> evt?
  make-evt : (-> any/c evt?)
procedure
(loop-evts make-evt ...+) -> evt?
  make-evt : (-> any/c evt?)
procedure
(serve  proc                    
       [#:on-stop on-stop       
        #:command handler]) -> process?
  proc : (-> msg/c msg/c)
  on-stop : (-> any) = void
  handler : (-> any) = void
procedure
(sink  proc                    
      [#:on-stop on-stop       
       #:command handler]) -> process?
  proc : (-> msg/c void?)
  on-stop : (-> any) = void
  handler : (-> any) = void
procedure
(source  proc                    
        [#:on-stop on-stop       
         #:command handler]) -> process?
  proc : (-> any/c)
  on-stop : (-> any) = void
  handler : (-> any) = void
procedure
(socket snk src [#:command handler]) -> process?
  snk : process?
  src : process?
  handler : (-> any) = void
procedure
(pipe π ... [#:command handler]) -> process?
  π : process?
  handler : (-> any) = void
procedure
(bridge π1 π2 [#:command handler]) -> process?
  π1 : process?
  π2 : process?
  handler : (-> any) = void
procedure
(proxy π [on-take on-emit #:command handler]) -> process?
  π : process?
  on-take : (-> msg/c msg/c) = values
  on-emit : (-> msg/c msg/c) = values
  handler : (-> any) = void
procedure
(managed  π                       
         [#:on-eof on-eof         
          #:on-stop on-stop       
          #:command handler]) -> process?
  π : process?
  on-eof : (-> any) = stop
  on-stop : (-> any) = void
  handler : (-> any) = void
procedure
(shutdown π) -> void?
  π : process?
procedure
(shutdown-evt π) -> evt?
  π : process?
value
msg/c : flat-contract?
