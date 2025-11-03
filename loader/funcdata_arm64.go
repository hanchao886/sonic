//go:build arm64 || arm || mips || mipsle || mips64 || mips64le || ppc64 || ppc64le || loong64 || riscv64 || s390x
// +build arm64 arm mips mipsle mips64 mips64le ppc64 ppc64le loong64 riscv64 s390x

/*
 * Copyright 2024 ByteDance Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package loader

// _MinLC defines the minimal instruction length code (pc quantum) for target arch group.
// Most RISC architectures use 4-byte aligned instructions
const _MinLC uint8 = 4
