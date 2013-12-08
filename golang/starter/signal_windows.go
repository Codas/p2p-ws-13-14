// starter
package main

import (
	"os"
	"os/exec"
	"syscall"
)

func setSysProcAttr(cmd *exec.Cmd) {
	cmd.SysProcAttr = &syscall.SysProcAttr{
		CreationFlags: syscall.CREATE_NEW_PROCESS_GROUP,
	}
}

// danke windows für die extra wurst
func sendInterruptSignal(p *os.Process) error {
	d, e := syscall.LoadDLL("kernel32.dll")
	if e != nil {
		return e
	}
	proc, e := d.FindProc("GenerateConsoleCtrlEvent")
	if e != nil {
		return e
	}
	r, _, e := proc.Call(syscall.CTRL_BREAK_EVENT, uintptr(p.Pid))
	if r == 0 {
		return e
	}
	return nil
}
