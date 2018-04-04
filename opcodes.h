// WARNING!!!!!!!!!!!!!!!!
// WARNING!!!!!!!!!!!!!!!!
// WARNING!!!!!!!!!!!!!!!!
// WARNING!!!!!!!!!!!!!!!!
// WARNING!!!!!!!!!!!!!!!!
// Order of these have to match the opCodeTypes array
OPCODE(LOADK)
// two arguments, The register to load into,
// the constant id.
OPCODE(LOAD_LOCAL)
// two arguments, The register to load into,
// the register that holds the symbol
OPCODE(SETUP_CALL)
// One argument, register holding Function/CFunction to call.
OPCODE(PUSH_ARG)
// In order, ie first one sets r0 in callee, second sets r1 etc.
OPCODE(CALL)
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

OPCODE(RETURN)
// One argument, the register holding the value to return.

OPCODE(CREATE_FUNC)
// two arguments, first a register
// second an immediate local function prototype id

OPCODE(MAKE_OBJECT)
// creates an empty object. One argument, the register to hold the object.

OPCODE(SET_OBJECT_SLOT)
// three arguments, the reg holding the object, the reg holding the key
// and the reg holding the value

OPCODE(DEFINE_GLOBAL)
// two arguments, the reg holding the value and the immediate symbol id
// that will become the variable

OPCODE(GET_GLOBAL)
// two arguments, the reg that will hold the value and the immediate
// symbol id refering to the symbol

OPCODE(RETURN_UNDEF)
// No arguments

OPCODE(GET_UPVALUE)
// two arguments, the reg to put the value in and the immediate
// upvalue index

OPCODE(JMP_IF_FALSE)
// two arguments, the reg that holds the predicate and the immediate
// relative jump address

OPCODE(DUMMY)
// dummy, used for patching

OPCODE(MOVE)
// two registers, dst, src

OPCODE(JMP)
// immediate relative jump address

OPCODE(LOAD_UNDEF)
// the register to load undef into

OPCODE(SET_UPVALUE)
// the reg holding the value to set it to and the immediate
// upvalue index

OPCODE(SET_GLOBAL)
// the reg holding the value to set it to and the immediate
// constant index of the symbol

OPCODE(CONS)
// three registers, dst, car, and cdr

OPCODE(LOAD_NULL)
// one register, dst