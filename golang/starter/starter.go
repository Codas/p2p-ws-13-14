// starter
package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"math/rand"
	"os"
	"os/exec"
	"runtime"
	"strconv"
	"strings"
	"sync"
	"syscall"
)

var (
	port      = flag.Int("startport", 11000, "first port of a range, where clients listen on")
	clients   = flag.Int("clients", 10, "Number of clients to start")
	locations = flag.Int("locations", 3, "Number of locations per client in ring")
)

var pool []*Client
var m = new(sync.RWMutex)

type Client struct {
	c    *exec.Cmd
	port int

	in *bufio.Writer
}

func main() {
	flag.Parse()

	fmt.Println("COMMANDS")
	fmt.Println("- q (QUIT)")
	fmt.Println("- l (list client ports)")
	fmt.Println("- n <#clients> (start <number of clients>)")
	fmt.Println("- s <#clients> (shutdown <number of clients>)")

	startupClients(*clients, *locations)
	parseStdIO()
}

func startupClients(clients, locations int) {
	for i := 0; i < clients; i++ {
		c, err := startupClient(*port)
		if err != nil {
			fmt.Fprintln(os.Stderr, "Error during client startup:", err)
			continue
		}

		m.Lock()
		pool = append(pool, c)
		m.Unlock()
		*port++
	}
}

func startupClient(port int) (c *Client, err error) {
	fmt.Printf("#%d Starting..\n", port)

	c = &Client{
		port: port,
	}

	c.c = exec.Command("runhaskell", "Server.hs", "-c", "-p", strconv.Itoa(port))

	outPipe, err := c.c.StdoutPipe()
	if err != nil {
		return nil, err
	}
	go printReader(port, outPipe, os.Stdout)

	inPipe, err := c.c.StdinPipe()
	if err != nil {
		return nil, err
	}
	c.in = bufio.NewWriter(inPipe)

	errPipe, err := c.c.StderrPipe()
	if err != nil {
		return nil, err
	}
	go printReader(port, errPipe, os.Stderr)

	go func() {
		err := c.c.Run()
		if err != nil {
			fmt.Printf("#%d Exited..\n", c.port)
		} else {
			fmt.Printf("#%d Exited (Error: %s)..\n", c.port, err)
		}
		removeClient(c)
	}()

	return c, nil
}

func printReader(port int, r io.Reader, w io.Writer) {
	scanner := bufio.NewScanner(r)

	for scanner.Scan() {
		text := scanner.Text()
		fmt.Fprintf(w, "[%d] %s\n", port, text)
	}
	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "[%d] Error: %s\n", port, err)
	}
}

func parseStdIO() {
	scanner := bufio.NewScanner(os.Stdin)

	for scanner.Scan() {
		text := scanner.Text()
		if idx := strings.Index(text, " "); idx != -1 {
			command := text[:idx]
			text = text[idx+1:]
			switch command {
			case "s":
				if i, err := strconv.Atoi(text); err == nil {
					shutdownClients(i)
				} else {
					fmt.Fprintln(os.Stderr, "Parsing error:", err)
				}
			case "n":
				if i, err := strconv.Atoi(text); err == nil {
					startupClients(i, *locations)
				} else {
					fmt.Fprintln(os.Stderr, "Parsing error:", err)
				}
			}
		} else {
			switch text {
			case "q":
				shutdownClients(-1)
				return
			case "l":
				listClients()
			}
		}
	}
	if err := scanner.Err(); err != nil {
		fmt.Fprintln(os.Stderr, "Error reading stdio:", err)
	}
}

func shutdownClients(num int) {
	m.RLock()
	defer m.RUnlock()

	if num > len(pool) || num < 0 {
		for _, c := range pool {
			shutdownClient(c)
		}
		return
	}

	// shutdown num random clients
	perm := rand.Perm(len(pool))
	for r := 0; r < num; r++ {
		c := pool[perm[r]]

		shutdownClient(c)
	}
}

func shutdownClient(c *Client) {
	fmt.Printf("#%d Sending Interrupt..\n", c.port)
	if err := sendInterruptSignal(c.c.Process); err != nil {
		fmt.Fprintln(os.Stderr, "#%d Error sending signal:", err)
	}
}

func removeClient(c *Client) {
	m.Lock()
	defer m.Unlock()
	if len(pool) == 0 {
		return
	}
	for i, cr := range pool {
		if c == cr {
			pool[i] = pool[len(pool)-1]
			pool = pool[:len(pool)-1]
		}
	}
}

func listClients() {
	m.RLock()
	defer m.RUnlock()

	if len(pool) == 0 {
		fmt.Println("No clients currently running")
		return
	}

	fmt.Println("Clients currently running on Ports:")
	for _, c := range pool {
		fmt.Print(c.port, " ")
	}
	fmt.Println()
}

func sendInterruptSignal(p *os.Process) error {
	// danke windows für die extra wurst
	if runtime.GOOS == "windows" {
		d, e := syscall.LoadDLL("kernel32.dll")
		if e != nil {
			return e
		}
		proc, e := d.FindProc("GenerateConsoleCtrlEvent")
		if e != nil {
			return e
		}
		_, _, e = proc.Call(syscall.CTRL_BREAK_EVENT, uintptr(p.Pid))
		return e
	} else {
		return p.Signal(os.Interrupt)
	}
}
