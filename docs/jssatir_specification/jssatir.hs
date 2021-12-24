import           Control.Applicative            ( Applicative )
import           Control.Monad                  ( ap
                                                , liftM
                                                )

newtype Register = Register Int deriving (Eq, Show)

data Value
    = ValHeap HeapIndex
    | ValNumber Int
    deriving (Eq, Show)

newtype RegMap = RegMap [(Register, Value)] deriving (Show)

newtype HeapIndex = HeapIndex Int deriving (Eq, Show)

data HeapValue
    = HeapRecord Record
    | HeapList List
    | HeapTuple Tuple
    deriving (Show)

newtype Record = Record [(Value, Value)] deriving (Show)
newtype List = List [Value] deriving (Show)
newtype Tuple = Tuple [Value] deriving (Show)

newtype Heap = Heap [HeapValue] deriving (Show)

data World = World RegMap Heap
    deriving Show

data RejectionReason = NotInSSAForm | RegisterUsedWithoutDeclaration | NeverAllocated deriving (Show)

data InvalidReason = NotHeapAllocated | TypeMismatch | MissingKey deriving (Show)

data Execution a
    = ExecRejected RejectionReason
    | ExecInvalid InvalidReason
    | ExecSuccess a
    deriving (Show)

data Instruction
    = InstNumberNew Register Int
    | InstRecordNew Register
    | InstRecordSet Register Register Register
    | InstRecordGet Register Register Register

instance Monad Execution where
    return x = ExecSuccess x

    ExecSuccess  value  >>= f = f value
    ExecInvalid  reason >>= f = ExecInvalid reason
    ExecRejected reason >>= f = ExecRejected reason

instance Functor Execution where
    fmap = liftM

instance Applicative Execution where
    pure  = return
    (<*>) = ap

newWorld :: World
newWorld = World (RegMap []) (Heap [])

isReg reg = (== reg) . fst

regsGet :: RegMap -> Register -> Execution Value
regsGet (RegMap values) reg = case filter (isReg reg) values of
    [(_, value)] -> ExecSuccess value
    []           -> ExecRejected RegisterUsedWithoutDeclaration
    (_ : _)      -> ExecRejected NotInSSAForm

regsInsert :: RegMap -> Register -> Value -> Execution RegMap
regsInsert (RegMap values) reg value
    | any (isReg reg) values = ExecRejected NotInSSAForm
    | otherwise              = ExecSuccess (RegMap (values ++ [(reg, value)]))

regsHeapIdx :: RegMap -> Register -> Execution HeapIndex
regsHeapIdx (RegMap values) reg = case map snd (filter (isReg reg) values) of
    [value] -> case value of
        ValHeap heap -> ExecSuccess heap
        _            -> ExecInvalid NotHeapAllocated
    (_ : _) -> ExecRejected NotInSSAForm
    []      -> ExecRejected RegisterUsedWithoutDeclaration

heapAlloc :: Heap -> HeapValue -> (Heap, HeapIndex)
heapAlloc (Heap elems) value =
    (Heap (elems ++ [value]), HeapIndex (length elems))

heapUpdate :: Heap -> HeapIndex -> HeapValue -> Heap
heapUpdate (Heap (_ : rest)) (HeapIndex 0) value = Heap (value : rest)
heapUpdate (Heap (x : rest)) (HeapIndex n) value =
    let (Heap tail) = heapUpdate (Heap rest) (HeapIndex (n - 1)) value
    in  Heap (x : tail)
heapUpdate (Heap []) _ _ =
    error "we can't update the heap if there's nothing to update"

heapRecord :: Heap -> HeapIndex -> Execution Record
heapRecord (Heap elems) index =
    case filter ((== index) . snd) (zip elems (map HeapIndex [0 ..])) of
        [(HeapRecord record, _)] -> ExecSuccess record
        [_                     ] -> ExecInvalid TypeMismatch
        []                       -> ExecRejected NeverAllocated
        (_ : _) ->
            error "cannot find more than one entry corresponding to heap index"

recordGet :: Record -> Value -> Execution Value
recordGet (Record elems) key = case filter ((== key) . fst) elems of
    [(_, value)] -> ExecSuccess value
    [] -> ExecInvalid MissingKey
    (_ : _) -> error "should never insert two of the same key into a record"

recordInsert :: Record -> (Value, Value) -> Record
recordInsert (Record values) (key, value) =
    Record (filter ((/= key) . fst) values ++ [(key, value)])

step :: World -> Instruction -> Execution World

step (World regs heap) (InstNumberNew register value) = do
    regs' <- regsInsert regs register (ValNumber value)
    return (World regs' heap)

step (World regs heap) (InstRecordNew register) = do
    let (heap', index) = heapAlloc heap (HeapRecord (Record []))

    regs' <- regsInsert regs register (ValHeap index)
    return (World regs' heap')

step (World regs heap) (InstRecordSet record key value) = do
    heapIdx <- regsHeapIdx regs record
    key'    <- regsGet regs key
    value'  <- regsGet regs value

    record  <- heapRecord heap heapIdx

    let record' = recordInsert record (key', value')
        heap'   = heapUpdate heap heapIdx (HeapRecord record')
    return (World regs heap')

step (World regs heap) (InstRecordGet register record key) = do
    heapIdx <- regsHeapIdx regs record
    key'    <- regsGet regs key

    record  <- heapRecord heap heapIdx
    value   <- recordGet record key'
    regs'   <- regsInsert regs register value

    return (World regs' heap)

stepAll :: World -> [Instruction] -> Execution World
stepAll world [] = ExecSuccess world
stepAll world (inst : rest) =
    step world inst >>= \world' -> stepAll world' rest

exampleSuccessFn =
    [ InstRecordNew (Register 0)
    , InstNumberNew (Register 1) 10
    , InstNumberNew (Register 2) 20
    , InstRecordSet (Register 0) (Register 1) (Register 2)
    , InstRecordGet (Register 3) (Register 0) (Register 1)
    ]

exampleInvalidFn =
    [ InstNumberNew (Register 0) 10
    , InstRecordGet (Register 1) (Register 0) (Register 0)
    ]

exampleRejectedFn =
    [InstNumberNew (Register 0) 10, InstNumberNew (Register 0) 20]

main = do
    print (stepAll newWorld exampleSuccessFn)
    print (stepAll newWorld exampleInvalidFn)
    print (stepAll newWorld exampleRejectedFn)
