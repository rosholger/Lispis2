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

OPCODE(LOAD_FUNC)
// two arguments, first a register second an immediate function id

OPCODE(MAKE_OBJECT)
// creates an empty object. One argument, the register to hold the object.

OPCODE(SET_OBJECT_SLOT)
// three arguments, the reg holding the object, the reg holding the key
// and the reg holding the value

OPCODE(DEFINE_GLOBAL)
// two arguments, the reg holding the value and the immediate symbol id
// that will become the variable

OPCODE(GET_GLOBAL)
// two arguments, the reg that will hold the value and the immediate symbol id
// refering to the symbol

OPCODE(RETURN_UNDEF)