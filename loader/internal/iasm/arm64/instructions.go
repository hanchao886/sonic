//
// Copyright 2024 CloudWeGo Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

package arm64

import "fmt"

// Data Processing Instructions

// ADD - Add (immediate or register)
func (p *Program) ADD(rd, rn Register, op interface{}) {
	insn := p.add("add", rd, rn, op)

	var sf uint32 = 1 // 64-bit by default
	if _, ok := rd.(Register32); ok {
		sf = 0
	}

	// Check if op is a register or immediate
	switch v := op.(type) {
	case Register64, Register32:
		// ADD (shifted register): sf 0 01011 shift 0 Rm imm6 Rn Rd
		// For simple ADD without shift: shift=00, imm6=000000
		rm := op.(Register)
		insn.enc = (sf << 31) | (0x0b << 24) | (encodeRegister(rm) << 16) | (encodeRegister(rn) << 5) | encodeRegister(rd)
	case int, int32, int64, uint32:
		// ADD (immediate): sf 0 010001 sh imm12 Rn Rd
		var immVal uint32
		switch iv := v.(type) {
		case int:
			immVal = encodeImm12(int64(iv))
		case int32:
			immVal = encodeImm12(int64(iv))
		case int64:
			immVal = encodeImm12(iv)
		case uint32:
			immVal = encodeImm12(int64(iv))
		}
		insn.enc = (sf << 31) | (0x11 << 24) | (immVal << 10) | (encodeRegister(rn) << 5) | encodeRegister(rd)
	default:
		panic(fmt.Sprintf("unsupported operand type for ADD: %T", op))
	}
}

// SUB - Subtract (immediate)
func (p *Program) SUB(rd, rn Register, imm interface{}) {
	insn := p.add("sub", rd, rn, imm)

	var immVal uint32
	switch v := imm.(type) {
	case int:
		immVal = encodeImm12(int64(v))
	case int32:
		immVal = encodeImm12(int64(v))
	case int64:
		immVal = encodeImm12(v)
	case uint32:
		immVal = encodeImm12(int64(v))
	default:
		panic(fmt.Sprintf("unsupported immediate type: %T", imm))
	}

	var sf uint32 = 1
	if _, ok := rd.(Register32); ok {
		sf = 0
	}

	// SUB (immediate): sf 1 010001 sh imm12 Rn Rd
	insn.enc = (sf << 31) | (1 << 30) | (0x11 << 24) | (immVal << 10) | (encodeRegister(rn) << 5) | encodeRegister(rd)
}

// SUBS - Subtract and set flags (shifted register)
func (p *Program) SUBS(rd, rn, rm Register) {
	insn := p.add("subs", rd, rn, rm)

	var sf uint32 = 1
	if _, ok := rd.(Register32); ok {
		sf = 0
	}

	// SUBS (shifted register): sf 11 01011 shift(00) 0 Rm imm6(000000) Rn Rd
	// shift=00 (LSL), imm6=0 (no shift)
	insn.enc = (sf << 31) | (0x6B << 24) | (encodeRegister(rm) << 16) | (encodeRegister(rn) << 5) | encodeRegister(rd)
}

// MOV - Move (register)
func (p *Program) MOV(rd, rn Register) {
	// MOV is an alias for ORR Rd, XZR, Rn
	insn := p.add("mov", rd, rn)

	var sf uint32 = 1
	if _, ok := rd.(Register32); ok {
		sf = 0
	}

	// ORR (shifted register): sf 01 01010 shift 0 Rm imm6 Rn Rd
	// With Rn = XZR and shift = 0, imm6 = 0, this becomes MOV
	zr := XZR
	if sf == 0 {
		zr = Register64(WZR)
	}
	insn.enc = (sf << 31) | (0x2a << 24) | (encodeRegister(rn) << 16) | (encodeRegister(zr) << 5) | encodeRegister(rd)
}

// MOVZ - Move wide with zero
func (p *Program) MOVZ(rd Register64, imm uint16, shift uint8) {
	insn := p.add("movz", rd, imm, shift)
	// MOVZ: sf 10 100101 hw imm16 Rd
	// For 64-bit: bits 31-23 = 110100101 = 0x1A5 << 1 | 1 << 8
	// Simpler: 0xD2800000 base + adjustments
	hw := uint32(shift / 16)
	if hw > 3 {
		panic(fmt.Sprintf("invalid shift for MOVZ: %d", shift))
	}
	// Base encoding for MOVZ X-reg: 0xD2800000
	// Add: hw field (bits 22-21) + imm16 (bits 20-5) + Rd (bits 4-0)
	insn.enc = 0xD2800000 | (hw << 21) | (uint32(imm) << 5) | encodeRegister(rd)
}

// MOVK - Move wide with keep
func (p *Program) MOVK(rd Register64, imm uint16, shift uint8) {
	insn := p.add("movk", rd, imm, shift)
	// MOVK: sf 11 100101 hw imm16 Rd
	// For 64-bit: 0xF2800000 base
	hw := uint32(shift / 16)
	if hw > 3 {
		panic(fmt.Sprintf("invalid shift for MOVK: %d", shift))
	}
	insn.enc = 0xF2800000 | (hw << 21) | (uint32(imm) << 5) | encodeRegister(rd)
}

// CMP - Compare (immediate)
func (p *Program) CMP(rn Register, imm interface{}) {
	// CMP is an alias for SUBS XZR, Rn, #imm
	insn := p.add("cmp", rn, imm)

	var immVal uint32
	switch v := imm.(type) {
	case int:
		immVal = encodeImm12(int64(v))
	case int32:
		immVal = encodeImm12(int64(v))
	case int64:
		immVal = encodeImm12(v)
	case uint32:
		immVal = encodeImm12(int64(v))
	default:
		panic(fmt.Sprintf("unsupported immediate type: %T", imm))
	}

	var sf uint32 = 1
	if _, ok := rn.(Register32); ok {
		sf = 0
	}

	zr := XZR
	if sf == 0 {
		zr = Register64(WZR)
	}

	// SUBS (immediate): sf 1 110001 sh imm12 Rn Rd
	insn.enc = (sf << 31) | (0x71 << 24) | (immVal << 10) | (encodeRegister(rn) << 5) | encodeRegister(zr)
}

// Load/Store Instructions

// LDR - Load register
func (p *Program) LDR(rt Register, mem *MemoryOperand) {
	insn := p.add("ldr", rt, mem)

	var size uint32
	switch rt.(type) {
	case Register64:
		size = 3 // 64-bit
	case Register32:
		size = 2 // 32-bit
	case DRegister:
		size = 3 // 64-bit FP
	case SRegister:
		size = 2 // 32-bit FP
	default:
		panic(fmt.Sprintf("unsupported register type for LDR: %T", rt))
	}

	insn.enc = encodeLoadStore(0x1, size, rt, mem)
}

// STR - Store register
func (p *Program) STR(rt Register, mem *MemoryOperand) {
	insn := p.add("str", rt, mem)

	var size uint32
	switch rt.(type) {
	case Register64:
		size = 3 // 64-bit
	case Register32:
		size = 2 // 32-bit
	case DRegister:
		size = 3 // 64-bit FP
	case SRegister:
		size = 2 // 32-bit FP
	default:
		panic(fmt.Sprintf("unsupported register type for STR: %T", rt))
	}

	insn.enc = encodeLoadStore(0x0, size, rt, mem)
}

// STP - Store pair
func (p *Program) STP(rt1, rt2 Register, mem *MemoryOperand) {
	insn := p.add("stp", rt1, rt2, mem)

	var opc uint32 // size field in ARM docs
	switch rt1.(type) {
	case Register64:
		opc = 2 // 10 = 64-bit pair
	case Register32:
		opc = 0 // 00 = 32-bit pair
	default:
		panic(fmt.Sprintf("unsupported register type for STP: %T", rt1))
	}

	// STP format: opc 101 V opc2 imm7 Rt2 Rn Rt1
	// V=0 for GPR, V=1 for FP/SIMD
	// opc2: 010=signed offset, 011=pre-index, 001=post-index
	var opc2 uint32
	if mem.Mode == AddrModePreIndex {
		opc2 = 0x3 // Pre-indexed: 011
	} else if mem.Mode == AddrModePostIndex {
		opc2 = 0x1 // Post-indexed: 001
	} else {
		opc2 = 0x2 // Signed offset: 010
	}

	// Scale: 32-bit pairs aligned to 4, 64-bit pairs aligned to 8
	scale := uint32(2)
	if opc == 2 {
		scale = 3 // 64-bit
	}

	offset := mem.Offset
	if offset%(1<<scale) != 0 {
		panic(fmt.Sprintf("STP offset must be %d-byte aligned", 1<<scale))
	}
	imm7 := uint32((offset >> scale) & 0x7F)

	// Encoding: opc(31-30) 101(29-27) V(26) opc2(24-22) imm7(21-15) Rt2(14-10) Rn(9-5) Rt1(4-0)
	vBit := uint32(0) // GPR

	rt1Enc := encodeRegister(rt1)
	rt2Enc := encodeRegister(rt2)
	rnEnc := encodeRegister(mem.Base)

	insn.enc = (opc << 30) | (0x5 << 27) | (vBit << 26) | (opc2 << 22) |
		(imm7 << 15) | (rt2Enc << 10) |
		(rnEnc << 5) | rt1Enc
}

// LDP - Load pair
func (p *Program) LDP(rt1, rt2 Register, mem *MemoryOperand) {
	insn := p.add("ldp", rt1, rt2, mem)

	var opc uint32 // size field
	switch rt1.(type) {
	case Register64:
		opc = 2 // 10 = 64-bit pair
	case Register32:
		opc = 0 // 00 = 32-bit pair
	default:
		panic(fmt.Sprintf("unsupported register type for LDP: %T", rt1))
	}

	// LDP format: opc 101 V opc2 imm7 Rt2 Rn Rt1
	// V=0 for GPR, opc2 for LDP has bit 0 set (vs STP)
	// opc2: 010=signed offset, 011=pre-index, 001=post-index
	// Actually for LDP, the encoding bit pattern in opc2 differs:
	// LDP uses the same pattern but with +1 on the load bit
	// Let me check: STP=010, LDP=011 for signed offset
	var opc2 uint32
	if mem.Mode == AddrModePreIndex {
		opc2 = 0x3 // Pre-indexed: 011
	} else if mem.Mode == AddrModePostIndex {
		opc2 = 0x1 // Post-indexed: 001
	} else {
		opc2 = 0x2 // Signed offset: 010 (for LDP, might need adjustment)
	}
	// Actually, looking at ARM manual, LDP also uses 010 for signed offset
	// The difference between STP and LDP is in bit 22 being included in "opc"
	// Let me reconsider: the docs show LDP as having the L bit set
	// bits 31-22: opc 101 V L opc2 where L=1 for load
	// So for signed offset: STP has 010, LDP has 011 at the opc2 position? No...
	// Actually ARM encoding shows:
	// STP: opc 101 0 010
	// LDP: opc 101 0 011  <- No, this is wrong
	// Let me check again... The bit 22 is part of addressing mode
	// For signed offset (neither pre nor post): bit pattern is X10 where X=0 for no writeback
	// Combined with load bit: STP = 0_10, LDP = 1_10
	// So the full field bits 24-22 is: 010 for STP signed, 011 for LDP pre-index...
	// I'm confusing myself. Let me look at actual encodings:
	// STP X29, X30, [SP, #16] = 0xA9027BFD = 10 101 0 010 0000010 11110 11111 11101
	// That's opc=10, fixed=101, V=0, opc2=010, imm7=0000010 (=2*8 bytes)
	// LDP X29, X30, [SP, #16] = 0xA9427BFD = 10 101 0 011 0000010 11110 11111 11101
	// That's opc=10, fixed=101, V=0, opc2=011
	// So LDP signed offset uses opc2=011, not 010!
	if mem.Mode != AddrModePreIndex && mem.Mode != AddrModePostIndex {
		opc2 = 0x3 // LDP signed offset uses 011
	}

	scale := uint32(2)
	if opc == 2 {
		scale = 3 // 64-bit
	}

	offset := mem.Offset
	if offset%(1<<scale) != 0 {
		panic(fmt.Sprintf("LDP offset must be %d-byte aligned", 1<<scale))
	}
	imm7 := uint32((offset >> scale) & 0x7F)

	// Encoding: opc(31-30) 101(29-27) V(26) opc2(24-22) imm7(21-15) Rt2(14-10) Rn(9-5) Rt1(4-0)
	vBit := uint32(0) // GPR
	insn.enc = (opc << 30) | (0x5 << 27) | (vBit << 26) | (opc2 << 22) |
		(imm7 << 15) | (encodeRegister(rt2) << 10) |
		(encodeRegister(mem.Base) << 5) | encodeRegister(rt1)
}

// Branch Instructions

// B - Unconditional branch
func (p *Program) B(label *Label) {
	insn := p.add("b", label)
	insn.label = label
	// Will be encoded during assembly when label position is known
	// B: 0 00101 imm26
	// For now, use a placeholder that will be fixed up
	insn.enc = 0x14000000 // B with offset 0
}

// BL - Branch with link
func (p *Program) BL(label *Label) {
	insn := p.add("bl", label)
	insn.label = label
	// BL: 1 00101 imm26
	insn.enc = 0x94000000 // BL with offset 0
}

// BR - Branch to register
func (p *Program) BR(rn Register64) {
	insn := p.add("br", rn)
	// BR: 1101011 0000 11111 000000 Rn 00000
	insn.enc = (0xd61f << 16) | (encodeRegister(rn) << 5)
}

// BLR - Branch with link to register
func (p *Program) BLR(rn Register64) {
	insn := p.add("blr", rn)
	// BLR: 1101011 0001 11111 000000 Rn 00000
	insn.enc = (0xd63f << 16) | (encodeRegister(rn) << 5)
}

// RET - Return
func (p *Program) RET() {
	insn := p.add("ret")
	// RET: 1101011 0010 11111 000000 11110 00000 (RET X30)
	insn.enc = 0xd65f03c0
}

// Conditional Branch

// Bcond - Conditional branch
func (p *Program) Bcond(cond Condition, label *Label) {
	insn := p.add(fmt.Sprintf("b.%s", cond), label)
	insn.label = label
	// B.cond: 01010100 imm19 0 cond
	insn.enc = 0x54000000 | encodeCondition(cond)
}

// Convenience methods for common conditions
func (p *Program) BEQ(label *Label) { p.Bcond(CondEQ, label) }
func (p *Program) BNE(label *Label) { p.Bcond(CondNE, label) }
func (p *Program) BLT(label *Label) { p.Bcond(CondLT, label) }
func (p *Program) BLE(label *Label) { p.Bcond(CondLE, label) }
func (p *Program) BGT(label *Label) { p.Bcond(CondGT, label) }
func (p *Program) BGE(label *Label) { p.Bcond(CondGE, label) }
func (p *Program) BLS(label *Label) { p.Bcond(CondLS, label) }
func (p *Program) BHI(label *Label) { p.Bcond(CondHI, label) }
func (p *Program) BLO(label *Label) { p.Bcond(CondLO, label) }

// Additional common instructions (merged from instructions_extra.go)

// AND - Bitwise AND (immediate)
func (p *Program) AND(rd, rn Register, imm uint64) {
	insn := p.add("and", rd, rn, imm)

	var sf uint32 = 1
	if _, ok := rd.(Register32); ok {
		sf = 0
	}

	// AND (immediate): sf 00 100100 N immr imms Rn Rd
	// Using simplified encoding - full implementation requires bitmask immediate encoding
	immEnc := encodeLogicalImmediate(imm, sf == 1)
	insn.enc = (sf << 31) | (0x24 << 23) | (immEnc << 10) | (encodeRegister(rn) << 5) | encodeRegister(rd)
}

// ORR - Bitwise OR (immediate)
func (p *Program) ORR(rd, rn Register, imm uint64) {
	insn := p.add("orr", rd, rn, imm)

	var sf uint32 = 1
	if _, ok := rd.(Register32); ok {
		sf = 0
	}

	// ORR (immediate): sf 01 100100 N immr imms Rn Rd
	immEnc := encodeLogicalImmediate(imm, sf == 1)
	insn.enc = (sf << 31) | (1 << 29) | (0x24 << 23) | (immEnc << 10) | (encodeRegister(rn) << 5) | encodeRegister(rd)
}

// EOR - Bitwise XOR (immediate)
func (p *Program) EOR(rd, rn Register, imm uint64) {
	insn := p.add("eor", rd, rn, imm)

	var sf uint32 = 1
	if _, ok := rd.(Register32); ok {
		sf = 0
	}

	// EOR (immediate): sf 10 100100 N immr imms Rn Rd
	immEnc := encodeLogicalImmediate(imm, sf == 1)
	insn.enc = (sf << 31) | (1 << 30) | (0x24 << 23) | (immEnc << 10) | (encodeRegister(rn) << 5) | encodeRegister(rd)
}

// LSL - Logical shift left (immediate)
func (p *Program) LSL(rd, rn Register, shift uint8) {
	insn := p.add("lsl", rd, rn, shift)

	var sf uint32 = 1
	var n uint32 = 64
	if _, ok := rd.(Register32); ok {
		sf = 0
		n = 32
	}

	if shift >= uint8(n) {
		panic("shift amount out of range")
	}

	// LSL is an alias for UBFM
	// UBFM: sf 10 100110 N immr imms Rn Rd
	immr := (n - uint32(shift)) & (n - 1)
	imms := n - uint32(shift) - 1

	insn.enc = (sf << 31) | (0x26 << 23) | (sf << 22) | (immr << 16) | (imms << 10) | (encodeRegister(rn) << 5) | encodeRegister(rd)
}

// LSR - Logical shift right (immediate)
func (p *Program) LSR(rd, rn Register, shift uint8) {
	insn := p.add("lsr", rd, rn, shift)

	var sf uint32 = 1
	var n uint32 = 64
	if _, ok := rd.(Register32); ok {
		sf = 0
		n = 32
	}

	if shift >= uint8(n) {
		panic("shift amount out of range")
	}

	// LSR is an alias for UBFM
	immr := uint32(shift)
	imms := n - 1

	insn.enc = (sf << 31) | (0x26 << 23) | (sf << 22) | (immr << 16) | (imms << 10) | (encodeRegister(rn) << 5) | encodeRegister(rd)
}

// ASR - Arithmetic shift right (immediate)
func (p *Program) ASR(rd, rn Register, shift uint8) {
	insn := p.add("asr", rd, rn, shift)

	var sf uint32 = 1
	var n uint32 = 64
	if _, ok := rd.(Register32); ok {
		sf = 0
		n = 32
	}

	if shift >= uint8(n) {
		panic("shift amount out of range")
	}

	// ASR is an alias for SBFM
	// SBFM: sf 00 100110 N immr imms Rn Rd
	immr := uint32(shift)
	imms := n - 1

	insn.enc = (sf << 31) | (0x13 << 23) | (sf << 22) | (immr << 16) | (imms << 10) | (encodeRegister(rn) << 5) | encodeRegister(rd)
}

// MUL - Multiply
func (p *Program) MUL(rd, rn, rm Register) {
	insn := p.add("mul", rd, rn, rm)

	var sf uint32 = 1
	if _, ok := rd.(Register32); ok {
		sf = 0
	}

	// MUL is an alias for MADD with Ra = XZR
	// MADD: sf 00 11011 000 Rm 0 Ra Rn Rd
	zr := XZR
	if sf == 0 {
		zr = Register64(WZR)
	}

	insn.enc = (sf << 31) | (0x1b << 24) | (encodeRegister(rm) << 16) | (encodeRegister(zr) << 10) | (encodeRegister(rn) << 5) | encodeRegister(rd)
}

// SDIV - Signed divide
func (p *Program) SDIV(rd, rn, rm Register) {
	insn := p.add("sdiv", rd, rn, rm)

	var sf uint32 = 1
	if _, ok := rd.(Register32); ok {
		sf = 0
	}

	// SDIV: sf 0 0 11010110 Rm 000011 Rn Rd
	insn.enc = (sf << 31) | (0x9a << 21) | (0x1 << 21) | (encodeRegister(rm) << 16) | (0x3 << 10) | (encodeRegister(rn) << 5) | encodeRegister(rd)
}

// UDIV - Unsigned divide
func (p *Program) UDIV(rd, rn, rm Register) {
	insn := p.add("udiv", rd, rn, rm)

	var sf uint32 = 1
	if _, ok := rd.(Register32); ok {
		sf = 0
	}

	// UDIV: sf 0 0 11010110 Rm 000010 Rn Rd
	insn.enc = (sf << 31) | (0x9a << 21) | (0x1 << 21) | (encodeRegister(rm) << 16) | (0x2 << 10) | (encodeRegister(rn) << 5) | encodeRegister(rd)
}

// NEG - Negate
func (p *Program) NEG(rd, rm Register) {
	insn := p.add("neg", rd, rm)

	var sf uint32 = 1
	if _, ok := rd.(Register32); ok {
		sf = 0
	}

	// NEG is an alias for SUB Rd, XZR, Rm
	zr := XZR
	if sf == 0 {
		zr = Register64(WZR)
	}

	// SUB (shifted register): sf 1 0 01011 shift 0 Rm imm6 Rn Rd
	insn.enc = (sf << 31) | (1 << 30) | (0xb << 24) | (encodeRegister(rm) << 16) | (encodeRegister(zr) << 5) | encodeRegister(rd)
}

// MVN - Bitwise NOT
func (p *Program) MVN(rd, rm Register) {
	insn := p.add("mvn", rd, rm)

	var sf uint32 = 1
	if _, ok := rd.(Register32); ok {
		sf = 0
	}

	// MVN is an alias for ORN Rd, XZR, Rm
	zr := XZR
	if sf == 0 {
		zr = Register64(WZR)
	}

	// ORN (shifted register): sf 01 01010 shift 1 Rm imm6 Rn Rd
	insn.enc = (sf << 31) | (1 << 29) | (0xa << 24) | (1 << 21) | (encodeRegister(rm) << 16) | (encodeRegister(zr) << 5) | encodeRegister(rd)
}

// CSEL - Conditional select
func (p *Program) CSEL(rd, rn, rm Register, cond Condition) {
	insn := p.add("csel", rd, rn, rm, cond)

	var sf uint32 = 1
	if _, ok := rd.(Register32); ok {
		sf = 0
	}

	// CSEL: sf 0 0 11010100 Rm cond 00 Rn Rd
	insn.enc = (sf << 31) | (0x9a << 21) | (encodeRegister(rm) << 16) | (encodeCondition(cond) << 12) | (encodeRegister(rn) << 5) | encodeRegister(rd)
}

// CSINC - Conditional select increment
func (p *Program) CSINC(rd, rn, rm Register, cond Condition) {
	insn := p.add("csinc", rd, rn, rm, cond)

	var sf uint32 = 1
	if _, ok := rd.(Register32); ok {
		sf = 0
	}

	// CSINC: sf 0 0 11010100 Rm cond 01 Rn Rd
	insn.enc = (sf << 31) | (0x9a << 21) | (encodeRegister(rm) << 16) | (encodeCondition(cond) << 12) | (1 << 10) | (encodeRegister(rn) << 5) | encodeRegister(rd)
}

// ADRP - Form PC-relative address to 4KB page
func (p *Program) ADRP(rd Register64, offset int32) {
	insn := p.add("adrp", rd, offset)

	// ADRP: 1 immlo 10000 immhi Rd
	// offset is in pages (4KB = 12 bits)
	pageOffset := offset >> 12
	immlo := uint32(pageOffset) & 0x3
	immhi := uint32(pageOffset>>2) & 0x7ffff

	insn.enc = (1 << 31) | (immlo << 29) | (0x10 << 24) | (immhi << 5) | encodeRegister(rd)
}

// ADR - Form PC-relative address
func (p *Program) ADR(rd Register64, offset int32) {
	insn := p.add("adr", rd, offset)

	// ADR: 0 immlo 10000 immhi Rd
	if offset < -(1<<20) || offset >= (1<<20) {
		panic("ADR offset out of range")
	}
	immlo := uint32(offset) & 0x3
	immhi := uint32(offset>>2) & 0x7ffff

	insn.enc = (immlo << 29) | (0x10 << 24) | (immhi << 5) | encodeRegister(rd)
}

// System Instructions

// MSR - Move to system register (simplified)
func (p *Program) MSR(sysreg string, rt Register64) {
	insn := p.add("msr", sysreg, rt)
	// This is simplified - full implementation needs system register encoding
	insn.enc = 0xd5000000 | encodeRegister(rt)
}

// MRS - Move from system register (simplified)
func (p *Program) MRS(rt Register64, sysreg string) {
	insn := p.add("mrs", rt, sysreg)
	// This is simplified - full implementation needs system register encoding
	insn.enc = 0xd5200000 | encodeRegister(rt)
}

// NEON and Floating-Point Instructions (merged from instructions_fp.go)

// FMOV - Floating-point move (register)
func (p *Program) FMOV(rd, rn Register) {
	insn := p.add("fmov", rd, rn)

	// Determine the type (S or D)
	var ftype uint32
	switch rd.(type) {
	case SRegister:
		ftype = 0 // 32-bit
	case DRegister:
		ftype = 1 // 64-bit
	default:
		panic("FMOV requires S or D registers")
	}

	// FMOV (register): 0 0 0 11110 type 1 00000 010000 Rn Rd
	insn.enc = (0x1e << 24) | (ftype << 22) | (1 << 21) | (0x40 << 10) |
		(encodeRegister(rn) << 5) | encodeRegister(rd)
}

// FMOV - Move between general and FP registers
func (p *Program) FMOVgen(rd, rn Register) {
	insn := p.add("fmov", rd, rn)

	// Determine direction and size
	var sf, ftype, rmode, opcode uint32

	switch {
	case isGPR64(rd) && isFPR64(rn):
		// FP to general (64-bit)
		sf, ftype, rmode, opcode = 1, 1, 0, 6
	case isFPR64(rd) && isGPR64(rn):
		// General to FP (64-bit)
		sf, ftype, rmode, opcode = 1, 1, 0, 7
	case isGPR32(rd) && isFPR32(rn):
		// FP to general (32-bit)
		sf, ftype, rmode, opcode = 0, 0, 0, 6
	case isFPR32(rd) && isGPR32(rn):
		// General to FP (32-bit)
		sf, ftype, rmode, opcode = 0, 0, 0, 7
	default:
		panic("Invalid FMOV operands")
	}

	// FMOV (general): sf 0 0 11110 type 1 rmode opcode 000000 Rn Rd
	insn.enc = (sf << 31) | (0x1e << 24) | (ftype << 22) | (1 << 21) |
		(rmode << 19) | (opcode << 16) | (encodeRegister(rn) << 5) | encodeRegister(rd)
}

// FADD - Floating-point add
func (p *Program) FADD(rd, rn, rm Register) {
	insn := p.add("fadd", rd, rn, rm)

	var ftype uint32
	switch rd.(type) {
	case SRegister:
		ftype = 0
	case DRegister:
		ftype = 1
	default:
		panic("FADD requires S or D registers")
	}

	// FADD: 0 0 0 11110 type 1 Rm 001010 Rn Rd
	insn.enc = (0x1e << 24) | (ftype << 22) | (1 << 21) | (encodeRegister(rm) << 16) |
		(0x2 << 12) | (0x2 << 10) | (encodeRegister(rn) << 5) | encodeRegister(rd)
}

// FSUB - Floating-point subtract
func (p *Program) FSUB(rd, rn, rm Register) {
	insn := p.add("fsub", rd, rn, rm)

	var ftype uint32
	switch rd.(type) {
	case SRegister:
		ftype = 0
	case DRegister:
		ftype = 1
	default:
		panic("FSUB requires S or D registers")
	}

	// FSUB: 0 0 0 11110 type 1 Rm 001110 Rn Rd
	insn.enc = (0x1e << 24) | (ftype << 22) | (1 << 21) | (encodeRegister(rm) << 16) |
		(0x3 << 12) | (0x2 << 10) | (encodeRegister(rn) << 5) | encodeRegister(rd)
}

// FMUL - Floating-point multiply
func (p *Program) FMUL(rd, rn, rm Register) {
	insn := p.add("fmul", rd, rn, rm)

	var ftype uint32
	switch rd.(type) {
	case SRegister:
		ftype = 0
	case DRegister:
		ftype = 1
	default:
		panic("FMUL requires S or D registers")
	}

	// FMUL: 0 0 0 11110 type 1 Rm 000010 Rn Rd
	insn.enc = (0x1e << 24) | (ftype << 22) | (1 << 21) | (encodeRegister(rm) << 16) |
		(0x0 << 12) | (0x2 << 10) | (encodeRegister(rn) << 5) | encodeRegister(rd)
}

// FDIV - Floating-point divide
func (p *Program) FDIV(rd, rn, rm Register) {
	insn := p.add("fdiv", rd, rn, rm)

	var ftype uint32
	switch rd.(type) {
	case SRegister:
		ftype = 0
	case DRegister:
		ftype = 1
	default:
		panic("FDIV requires S or D registers")
	}

	// FDIV: 0 0 0 11110 type 1 Rm 000110 Rn Rd
	insn.enc = (0x1e << 24) | (ftype << 22) | (1 << 21) | (encodeRegister(rm) << 16) |
		(0x1 << 12) | (0x2 << 10) | (encodeRegister(rn) << 5) | encodeRegister(rd)
}

// FCMP - Floating-point compare
func (p *Program) FCMP(rn, rm Register) {
	insn := p.add("fcmp", rn, rm)

	var ftype uint32
	switch rn.(type) {
	case SRegister:
		ftype = 0
	case DRegister:
		ftype = 1
	default:
		panic("FCMP requires S or D registers")
	}

	// FCMP: 0 0 0 11110 type 1 Rm 001000 Rn 00000
	insn.enc = (0x1e << 24) | (ftype << 22) | (1 << 21) | (encodeRegister(rm) << 16) |
		(0x2 << 12) | (encodeRegister(rn) << 5)
}

// FCVT - Floating-point convert (between precisions)
func (p *Program) FCVT(rd, rn Register) {
	insn := p.add("fcvt", rd, rn)

	var ftype, opc uint32

	switch {
	case isFPR32(rd) && isFPR64(rn):
		// D to S
		ftype, opc = 1, 0
	case isFPR64(rd) && isFPR32(rn):
		// S to D
		ftype, opc = 0, 1
	default:
		panic("Invalid FCVT operands")
	}

	// FCVT: 0 0 0 11110 type 1 opc 10000 10000 Rn Rd
	insn.enc = (0x1e << 24) | (ftype << 22) | (1 << 21) | (opc << 15) |
		(0x10 << 10) | (0x10 << 5) | (encodeRegister(rn) << 5) | encodeRegister(rd)
}

// FCVTZS - Floating-point convert to signed integer
func (p *Program) FCVTZS(rd Register, rn Register) {
	insn := p.add("fcvtzs", rd, rn)

	var sf, ftype uint32

	switch {
	case isGPR64(rd) && isFPR64(rn):
		sf, ftype = 1, 1
	case isGPR32(rd) && isFPR32(rn):
		sf, ftype = 0, 0
	case isGPR64(rd) && isFPR32(rn):
		sf, ftype = 1, 0
	case isGPR32(rd) && isFPR64(rn):
		sf, ftype = 0, 1
	default:
		panic("Invalid FCVTZS operands")
	}

	// FCVTZS: sf 0 0 11110 type 1 11 000 000000 Rn Rd
	insn.enc = (sf << 31) | (0x1e << 24) | (ftype << 22) | (1 << 21) |
		(0x18 << 16) | (encodeRegister(rn) << 5) | encodeRegister(rd)
}

// SCVTF - Signed integer convert to floating-point
func (p *Program) SCVTF(rd Register, rn Register) {
	insn := p.add("scvtf", rd, rn)

	var sf, ftype uint32

	switch {
	case isFPR64(rd) && isGPR64(rn):
		sf, ftype = 1, 1
	case isFPR32(rd) && isGPR32(rn):
		sf, ftype = 0, 0
	case isFPR64(rd) && isGPR32(rn):
		sf, ftype = 0, 1
	case isFPR32(rd) && isGPR64(rn):
		sf, ftype = 1, 0
	default:
		panic("Invalid SCVTF operands")
	}

	// SCVTF: sf 0 0 11110 type 1 00 010 000000 Rn Rd
	insn.enc = (sf << 31) | (0x1e << 24) | (ftype << 22) | (1 << 21) |
		(0x2 << 16) | (encodeRegister(rn) << 5) | encodeRegister(rd)
}

// Helper functions to check register types
func isGPR64(r Register) bool {
	_, ok := r.(Register64)
	return ok
}

func isGPR32(r Register) bool {
	_, ok := r.(Register32)
	return ok
}

func isFPR64(r Register) bool {
	_, ok := r.(DRegister)
	return ok
}

func isFPR32(r Register) bool {
	_, ok := r.(SRegister)
	return ok
}

func isFPR128(r Register) bool {
	_, ok := r.(VRegister)
	return ok
}
