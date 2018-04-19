OPCODE(LOADK, RI)
// two arguments, The register to load into,
// the constant id.
OPCODE(LOAD_LOCAL, RR)
// two arguments, The register to load into,
// the register that holds the symbol
OPCODE(SETUP_CALL, R)
// One argument, register holding Function/CFunction to call.
OPCODE(PUSH_ARG, R)
// In order, ie first one sets r0 in callee, second sets r1 etc.
OPCODE(CALL, R)
// One argument, the return register.
// Calls look like:
// OP_SETUP_CALL r9
// OP_PUSH_ARG r5
// OP_PUSH_ARG r6
// OP_PUSH_ARG r7
// OP_PUSH_ARG r1
// OP_CALL
// WARNING:
// WARNING:
// WARNING:
// WARNING:
// WARNING: No other Opcodes are legal between OP_SETUP_CALL and
// OP_CALL

OPCODE(RETURN, R)
// One argument, the register holding the value to return.

OPCODE(CREATE_FUNC, RI)
// two arguments, first a register
// second an immediate local function prototype id

OPCODE(MAKE_OBJECT, R)
// creates an empty object. One argument, the register to hold the object.

OPCODE(SET_OBJECT_SLOT, RRR)
// three arguments, the reg holding the object, the reg holding the key
// and the reg holding the value

OPCODE(DEFINE_GLOBAL, RI)
// two arguments, the reg holding the value and the immediate symbol id
// that will become the variable

OPCODE(GET_GLOBAL, RI)
// two arguments, the reg that will hold the value and the immediate
// symbol id refering to the symbol

OPCODE(RETURN_UNDEF, N)
// No arguments

OPCODE(GET_UPVALUE, RI)
// two arguments, the reg to put the value in and the immediate
// upvalue index

OPCODE(JMP_IF_FALSE, RS)
// two arguments, the reg that holds the predicate and the immediate
// relative jump address

OPCODE(DUMMY, N)
// dummy, used for patching

OPCODE(MOVE, RR)
// two registers, dst, src

OPCODE(JMP, S)
// immediate relative jump address

OPCODE(LOAD_UNDEF, R)
// the register to load undef into

OPCODE(SET_UPVALUE, RI)
// the reg holding the value to set it to and the immediate
// upvalue index

OPCODE(SET_GLOBAL, RI)
// the reg holding the value to set it to and the immediate
// constant index of the symbol

OPCODE(CONS, RRR)
// three registers, dst, car, and cdr

OPCODE(LOAD_NULL, R)
// one register, dst

OPCODE(CRASH, R)
// The crashvalue to "return"