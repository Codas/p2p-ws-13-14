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
	"strings"
	"sync"
	"time"
)

var (
	port       = flag.Int("p", 9000, "first port of a range, where clients listen on")
	clients    = flag.Int("c", 3, "Number of clients to start")
	locations  = flag.Int("l", 1, "Number of locations per client in ring")
	executable = flag.String("x", "peer.exe", "Executable file that starts a peer")
	graph      = flag.Bool("g", false, "Let first Peer execute a graph generation")
	delay      = flag.Int("d", 0, "Delay in ms between starting of peers")
)

var pool []*Client
var m = new(sync.RWMutex)
var shuttingDown = false
var done = make(chan bool)

type Client struct {
	cmd  *exec.Cmd
	port int

	in *bufio.Writer
}

func (c *Client) sendCommand(cmd string) {
	fmt.Printf("#%d >> %s\n", c.port, cmd)
	c.in.WriteString(cmd)
	c.in.WriteString("\n")
	c.in.Flush()
}

func main() {
	flag.Parse()

	startupClients(*clients, *locations)
	consoleLoop()

	// shut down
	m.Lock()
	if len(pool) == 0 {
		return
	}
	shuttingDown = true
	m.Unlock()
	shutdownClients(-1)
	<-done
}

func consoleLoop() {
	scanner := bufio.NewScanner(os.Stdin)

	for scanner.Scan() {
		text := scanner.Text()
		command := text
		args := ""
		if idx := strings.Index(text, " "); idx != -1 {
			command = text[:idx]
			args = text[idx+1:]
		}
		switch command {
		case "h":
			printHelp()
		case "q":
			return
		case "l":
			listClients()
		case "n":
			var num int
			if _, err := fmt.Sscan(args, &num); err != nil {
				fmt.Fprintf(os.Stderr, "Error: parameter needs to have form '<num>' (%s)\n", err)
				return
			}
			startupClients(num, *locations)
		case "s":
			var num int
			if _, err := fmt.Sscan(args, &num); err != nil {
				fmt.Fprintf(os.Stderr, "Error: parameter needs to have form '<num>' (%s)\n", err)
				return
			}
			shutdownClients(num)
		case "c":
			var port int
			var peercmd string
			if _, err := fmt.Sscanf(args, "%d %s", &port, &peercmd); err != nil {
				fmt.Fprintf(os.Stderr, "Error: parameter needs to have form '<port> <command>' (%s)\n", err)
				return
			}
			sendCommandtoClient(port, peercmd)
		}
	}
	if err := scanner.Err(); err != nil {
		fmt.Fprintln(os.Stderr, "Error reading stdio:", err)
	}
}

func printHelp() {
	fmt.Println("HELP")
	fmt.Println("- h (print this help)")
	fmt.Println("- q (QUIT)")
	fmt.Println("- l (list peer ports)")
	fmt.Println("- n <num> (start <num> new peers>)")
	fmt.Println("- s <num> (shutdown <num> peers)")
	fmt.Println("- c <port> <cmd> (send <cmd> to peer with <port>)")
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
		if i != clients-1 {
			time.Sleep(time.Duration(*delay) * time.Millisecond)
		}
	}
}

func startupClient(port int) (c *Client, err error) {
	c = &Client{
		port: port,
	}

	fmt.Printf("#%d Starting [%s -p %d] ..\n", port, *executable, port)
	c.cmd = exec.Command(*executable, "-p", fmt.Sprint(port))
	setSysProcAttr(c.cmd)

	outPipe, err := c.cmd.StdoutPipe()
	if err != nil {
		return nil, err
	}
	go printReader(port, outPipe, os.Stdout)

	inPipe, err := c.cmd.StdinPipe()
	if err != nil {
		return nil, err
	}
	c.in = bufio.NewWriter(inPipe)

	errPipe, err := c.cmd.StderrPipe()
	if err != nil {
		return nil, err
	}
	go printReader(port, errPipe, os.Stderr)

	go func() {
		err := c.cmd.Run()
		if err != nil {
			fmt.Printf("#%d Exited (Error: %s)..\n", c.port, err)
		} else {
			fmt.Printf("#%d Exited..\n", c.port)
		}
		removeClient(c)
	}()

	m.RLock()
	if len(pool) == 0 {
		c.sendCommand("cycle")
	}
	if len(pool) == 0 && *graph {
		c.sendCommand("gf")
	}
	if len(pool) > 0 {
		for i := 0; i < *locations; i++ {
			p := pool[rand.Intn(len(pool))]
			c.sendCommand(fmt.Sprintf("c :%d", p.port))
		}
	}
	m.RUnlock()

	return c, nil
}

func printReader(port int, r io.Reader, w io.Writer) {
	scanner := bufio.NewScanner(r)

	for scanner.Scan() {
		text := scanner.Text()
		fmt.Fprintf(w, "[%d] %s\n", port, text)
	}
}

func sendCommandtoClient(port int, cmd string) {
	m.RLock()
	defer m.RUnlock()

	for _, c := range pool {
		if c.port == port {
			c.sendCommand(cmd)
			break
		}
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
	/*
		fmt.Printf("#%d Sending Interrupt..\n", c.port)
		if err := sendInterruptSignal(c.cmd.Process); err != nil {
			fmt.Fprintf(os.Stderr, "#%d Error sending signal: %s\n", c.port, err)
		}
	*/
	c.sendCommand("q")
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
	if shuttingDown && len(pool) == 0 {
		done <- true
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
