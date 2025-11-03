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

package arm64

import "testing"

func TestLogicalImmediateEncodings(t *testing.T) {
	p := NewProgram()

	p.AND(X0, X1, 0xff)
	expAND := (uint32(1) << 31) | (0x24 << 23) | (encodeRegister(X1) << 5) | encodeRegister(X0)
	if p.insns[0].enc != expAND {
		t.Errorf("AND encoding: got 0x%08x, want 0x%08x (imm field simplified)", p.insns[0].enc, expAND)
	}

	p = NewProgram()
	p.ORR(W2, W3, 0x7)
	expORR := (uint32(0) << 31) | (1 << 29) | (0x24 << 23) | (encodeRegister(W3) << 5) | encodeRegister(W2)
	if p.insns[0].enc != expORR {
		t.Errorf("ORR encoding: got 0x%08x, want 0x%08x", p.insns[0].enc, expORR)
	}

	p = NewProgram()
	p.EOR(X4, X5, 0x1)
	expEOR := (uint32(1) << 31) | (1 << 30) | (0x24 << 23) | (encodeRegister(X5) << 5) | encodeRegister(X4)
	if p.insns[0].enc != expEOR {
		t.Errorf("EOR encoding: got 0x%08x, want 0x%08x", p.insns[0].enc, expEOR)
	}
}

func TestShiftRangePanics(t *testing.T) {
	p := NewProgram()
	defer func() {
		if r := recover(); r == nil {
			t.Errorf("expected panic for LSL out of range")
		}
	}()
	p.LSL(W0, W1, 32) // out of range for 32-bit
}

func TestNEG_MVN_Encodings(t *testing.T) {
	p := NewProgram()
	p.NEG(X0, X1)
	// Rn must be XZR
	if rn := (p.insns[0].enc >> 5) & 0x1f; rn != uint32(XZR.RegIndex()) {
		t.Errorf("NEG Rn field = %d, want XZR(%d)", rn, XZR.RegIndex())
	}

	p = NewProgram()
	p.MVN(W2, W3)
	// Rn must be WZR (encoded as 31)
	if rn := (p.insns[0].enc >> 5) & 0x1f; rn != uint32(WZR.RegIndex()) {
		t.Errorf("MVN Rn field = %d, want WZR(%d)", rn, WZR.RegIndex())
	}
}

func TestDIV_EncodesRmRnRd(t *testing.T) {
	p := NewProgram()
	p.SDIV(X0, X1, X2)
	ins := p.insns[0].enc
	if rd := ins & 0x1f; rd != uint32(X0.RegIndex()) {
		t.Errorf("SDIV Rd=%d", rd)
	}
	if rn := (ins >> 5) & 0x1f; rn != uint32(X1.RegIndex()) {
		t.Errorf("SDIV Rn=%d", rn)
	}
	if rm := (ins >> 16) & 0x1f; rm != uint32(X2.RegIndex()) {
		t.Errorf("SDIV Rm=%d", rm)
	}

	p = NewProgram()
	p.UDIV(W3, W4, W5)
	ins = p.insns[0].enc
	if rd := ins & 0x1f; rd != uint32(W3.RegIndex()) {
		t.Errorf("UDIV Rd=%d", rd)
	}
	if rn := (ins >> 5) & 0x1f; rn != uint32(W4.RegIndex()) {
		t.Errorf("UDIV Rn=%d", rn)
	}
	if rm := (ins >> 16) & 0x1f; rm != uint32(W5.RegIndex()) {
		t.Errorf("UDIV Rm=%d", rm)
	}
}

func TestCSEL_CSINC_Cond(t *testing.T) {
	p := NewProgram()
	p.CSEL(X0, X1, X2, CondEQ)
	if cond := (p.insns[0].enc >> 12) & 0xf; cond != uint32(CondEQ) {
		t.Errorf("CSEL cond=%d, want EQ(%d)", cond, CondEQ)
	}

	p = NewProgram()
	p.CSINC(W0, W1, W2, CondGT)
	if cond := (p.insns[0].enc >> 12) & 0xf; cond != uint32(CondGT) {
		t.Errorf("CSINC cond=%d, want GT(%d)", cond, CondGT)
	}
}

func TestADR_ADRP(t *testing.T) {
	p := NewProgram()
	p.ADR(X0, 4)                                         // small positive offset
	if op := (p.insns[0].enc >> 24) & 0x1f; op != 0x10 { // 10000b
		t.Errorf("ADR op field got 0x%x", op)
	}

	p = NewProgram()
	p.ADRP(X1, 0x1000) // one page
	if bit31 := (p.insns[0].enc >> 31) & 1; bit31 != 1 {
		t.Errorf("ADRP bit31 should be 1, got %d", bit31)
	}
}

func TestFMOV_Register(t *testing.T) {
	p := NewProgram()
	p.FMOV(D0, D1)
	if rd := p.insns[0].enc & 0x1f; rd != uint32(D0.RegIndex()) {
		t.Errorf("FMOV rd=%d", rd)
	}
}

func TestFMOVgen_Directions(t *testing.T) {
	p := NewProgram()
	// FP -> GPR 64-bit
	p.FMOVgen(X0, D1)
	if rd := p.insns[0].enc & 0x1f; rd != uint32(X0.RegIndex()) {
		t.Errorf("FMOVgen rd=%d", rd)
	}
	if rn := (p.insns[0].enc >> 5) & 0x1f; rn != uint32(D1.RegIndex()) {
		t.Errorf("FMOVgen rn=%d", rn)
	}

	p = NewProgram()
	// GPR -> FP 32-bit
	p.FMOVgen(S2, W3)
	if rd := p.insns[0].enc & 0x1f; rd != uint32(S2.RegIndex()) {
		t.Errorf("FMOVgen rd=%d", rd)
	}
	if rn := (p.insns[0].enc >> 5) & 0x1f; rn != uint32(W3.RegIndex()) {
		t.Errorf("FMOVgen rn=%d", rn)
	}
}

func TestFCVT_FCVTZS_SCVTF(t *testing.T) {
	p := NewProgram()
	p.FCVT(D0, S1) // S->D
	if rd := p.insns[0].enc & 0x1f; rd != uint32(D0.RegIndex()) {
		t.Errorf("FCVT rd=%d", rd)
	}

	p = NewProgram()
	p.FCMP(D2, D3)
	if rn := (p.insns[0].enc >> 5) & 0x1f; rn != uint32(D2.RegIndex()) {
		t.Errorf("FCMP rn=%d", rn)
	}

	p = NewProgram()
	p.FCVTZS(X4, S5)
	if rd := p.insns[0].enc & 0x1f; rd != uint32(X4.RegIndex()) {
		t.Errorf("FCVTZS rd=%d", rd)
	}

	p = NewProgram()
	p.SCVTF(D6, X7)
	if rd := p.insns[0].enc & 0x1f; rd != uint32(D6.RegIndex()) {
		t.Errorf("SCVTF rd=%d", rd)
	}
}

func TestMOVZ_MOVK(t *testing.T) {
	p := NewProgram()
	// MOVZ X0, #0x1234, LSL #0
	p.MOVZ(X0, 0x1234, 0)
	ins := p.insns[0].enc
	if rd := ins & 0x1f; rd != uint32(X0.RegIndex()) {
		t.Errorf("MOVZ Rd=%d, want %d", rd, X0.RegIndex())
	}
	// Check it's 64-bit (sf=1)
	if sf := (ins >> 31) & 1; sf != 1 {
		t.Errorf("MOVZ sf=%d, want 1 for X register", sf)
	}
	// Check opcode field for MOVZ (opc=10)
	if opc := (ins >> 29) & 0x3; opc != 0x2 {
		t.Errorf("MOVZ opc=%d, want 2", opc)
	}

	p = NewProgram()
	// MOVK X1, #0x5678, LSL #16
	p.MOVK(X1, 0x5678, 16)
	ins = p.insns[0].enc
	if rd := ins & 0x1f; rd != uint32(X1.RegIndex()) {
		t.Errorf("MOVK Rd=%d, want %d", rd, X1.RegIndex())
	}
	// Check opcode field for MOVK (opc=11)
	if opc := (ins >> 29) & 0x3; opc != 0x3 {
		t.Errorf("MOVK opc=%d, want 3", opc)
	}
	// Check hw field (shift/16)
	if hw := (ins >> 21) & 0x3; hw != 1 {
		t.Errorf("MOVK hw=%d, want 1 for LSL #16", hw)
	}
}

func TestConditionalBranches(t *testing.T) {
	// Test all conditional branch variants
	tests := []struct {
		name string
		cond Condition
	}{
		{"BEQ", CondEQ},
		{"BNE", CondNE},
		{"BLT", CondLT},
		{"BLE", CondLE},
		{"BGT", CondGT},
		{"BGE", CondGE},
		{"BLS", CondLS},
		{"BHI", CondHI},
	}

	for _, tt := range tests {
		p := NewProgram()
		label := CreateLabel(tt.name)

		// Call the appropriate branch function
		switch tt.cond {
		case CondEQ:
			p.BEQ(label)
		case CondNE:
			p.BNE(label)
		case CondLT:
			p.BLT(label)
		case CondLE:
			p.BLE(label)
		case CondGT:
			p.BGT(label)
		case CondGE:
			p.BGE(label)
		case CondLS:
			p.BLS(label)
		case CondHI:
			p.BHI(label)
		}

		ins := p.insns[0].enc
		// Check condition code field (bits 0-3)
		if gotCond := ins & 0xf; gotCond != uint32(tt.cond) {
			t.Errorf("%s: cond=%d, want %d", tt.name, gotCond, tt.cond)
		}
		// Check opcode for conditional branch (bits 24-31 should be 0x54)
		if op := (ins >> 24) & 0xff; (op & 0xfe) != 0x54 {
			t.Errorf("%s: opcode=0x%x, want 0x54/0x55", tt.name, op)
		}
	}
}

func TestMSR_MRS(t *testing.T) {
	p := NewProgram()
	// MSR NZCV, X0
	p.MSR("nzcv", X0)
	ins := p.insns[0].enc
	// Check it's a system register instruction
	if op0 := (ins >> 19) & 0x3; op0 != 0 {
		t.Errorf("MSR op0=%d", op0)
	}
	// Verify Rt field
	if rt := ins & 0x1f; rt != uint32(X0.RegIndex()) {
		t.Errorf("MSR Rt=%d, want %d", rt, X0.RegIndex())
	}

	p = NewProgram()
	// MRS X1, NZCV
	p.MRS(X1, "nzcv")
	ins = p.insns[0].enc
	// Verify Rt field
	if rt := ins & 0x1f; rt != uint32(X1.RegIndex()) {
		t.Errorf("MRS Rt=%d, want %d", rt, X1.RegIndex())
	}
}

func TestASR_Encoding(t *testing.T) {
	p := NewProgram()
	// ASR X0, X1, #5
	p.ASR(X0, X1, 5)
	ins := p.insns[0].enc

	// Verify Rd
	if rd := ins & 0x1f; rd != uint32(X0.RegIndex()) {
		t.Errorf("ASR Rd=%d, want %d", rd, X0.RegIndex())
	}
	// Verify Rn
	if rn := (ins >> 5) & 0x1f; rn != uint32(X1.RegIndex()) {
		t.Errorf("ASR Rn=%d, want %d", rn, X1.RegIndex())
	}
	// Check it's 64-bit (sf=1)
	if sf := (ins >> 31) & 1; sf != 1 {
		t.Errorf("ASR sf=%d, want 1 for X register", sf)
	}

	p = NewProgram()
	// ASR W2, W3, #10
	p.ASR(W2, W3, 10)
	ins = p.insns[0].enc
	// Check it's 32-bit (sf=0)
	if sf := (ins >> 31) & 1; sf != 0 {
		t.Errorf("ASR sf=%d, want 0 for W register", sf)
	}
}
