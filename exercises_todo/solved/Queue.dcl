definition module Queue

:: Queue a
// a queue is a FIFO data structure - first element in - first element out

newQueue ::     (Queue a)              // creates an empty queue
empty    ::     (Queue a) -> Bool      // checks if the queue is empty
push     ::  a  (Queue a) -> Queue a   // inserts a new element at the end of the queue
pushes   :: [a] (Queue a) -> Queue a   // consecutively inserts new elements to queue
pop      ::     (Queue a) -> Queue a   // removes an element from the queue
popn     :: Int (Queue a) -> Queue a   // removes the top n elements from the queue
top      ::     (Queue a) -> a         // returns the next to be deleted element of the queue
topn     :: Int (Queue a) -> [a]       // returns the top n elements of the queue
elements ::     (Queue a) -> [a]       // returns all the elements of the queue in a list
count    ::     (Queue a) -> Int       // counts the number of elements of the queue
