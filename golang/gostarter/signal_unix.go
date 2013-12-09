// signal handling

// +build darwin linux

package main

import (
	"os"
	"os/exec"
)

func setSysProcAttr(cmd *exec.Cmd) {}

func sendInterruptSignal(p *os.Process) error {
	return p.Signal(os.Interrupt)
}
