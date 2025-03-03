For load use load.lisp
--
--

Alpabet:
 adds new sintax in []

  (setf *x* 4)
   [ a 'b (var x) 7 ] -> (alphabet #\a b 4 7) 
   
   [ a-z ] -> (alphabet #\a #\b #\c .... #\z) 
   
   [:a :b] -> (alphabet #\A #\B) 
   
   [1-5 0] -> (alphabet #\1 #\2 ... #\5 #\0)
   
   setf *numbers* [0-9]
   
   setf *downcase* [a-z]
   
   [ (var *numbers*) (var *downcase*) ] -> alphabet with all numbers and downcase letters
   
   same as [o-9 a-z] or [[0-9] [a-z]]

Macro defautomaton

(defautomaton name ( alphabet :as alpabet_name (defaults to sigma)) @body states)

state: 
      ( name (t or nil)

        ( alphabet -> new_state)
            ...
        ( alphabet -> new_state))

Must contains state named delta as a starting point
 
