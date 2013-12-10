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
	"strconv"
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
	c.in.WriteString(cmd)
	c.in.WriteString("\n")
	c.in.Flush()
	fmt.Printf("#%d >> %s\n", c.port, cmd)
}

func main() {
	flag.Parse()

	fmt.Println("COMMANDS")
	fmt.Println("- q (QUIT)")
	fmt.Println("- l (list client ports)")
	fmt.Println("- n <#clients> (start <number of clients>)")
	fmt.Println("- s <#clients> (shutdown <number of clients>)")
	fmt.Println("- c <#port> <cmd> (send <cmd> to client with <#port>)")

	startupClients(*clients, *locations)
	parseStdIO()

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
	c.cmd = exec.Command(*executable, "-p", strconv.Itoa(port))
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
			c.sendCommand("c 127.0.0.1 " + strconv.Itoa(p.port))
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
			case "c":
				idx = strings.Index(text, " ")
				if idx == -1 {
					fmt.Fprintln(os.Stderr, "Malformed command")
				}
				if i, err := strconv.Atoi(text[:idx]); err == nil {
					sendCommandtoClient(i, text[idx+1:])
				} else {
					fmt.Fprintln(os.Stderr, "Parsing error:", err)
				}

			}
		} else {
			switch text {
			case "q":
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
