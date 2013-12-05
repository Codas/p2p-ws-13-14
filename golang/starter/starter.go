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
)

var (
	port      = flag.Int("startport", 11000, "first port of a range, where clients listen on")
	clients   = flag.Int("clients", 10, "Number of clients to start")
	locations = flag.Int("locations", 3, "Number of locations per client in ring")
)

var pool []*Client

type Client struct {
	c    *exec.Cmd
	port int

	in *bufio.Writer

	closed chan<- error
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

		pool = append(pool, c)
		*port++
	}
}

func startupClient(port int) (c *Client, err error) {
	fmt.Printf("[%d] Starting..\n", port)

	c = &Client{
		port:   port,
		closed: make(chan error, 1),
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

	go func(err chan<- error) {
		err <- c.c.Run()
	}(c.closed)

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
				shutdownClients(len(pool))
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
	if num > len(pool) {
		num = len(pool)
	}
	for i := 0; i < num; i++ {
		r := rand.Intn(len(pool))
		c := pool[r]

		fmt.Printf("[%d] Shutting down..\n", c.port)
		//if err := c.c.Process.Signal(os.Interrupt); err != nil {
		if err := c.c.Process.Signal(os.Kill); err != nil {
			fmt.Fprintln(os.Stderr, "Error sending signal:", err)
		}

		pool[r] = pool[len(pool)-1]
		pool = pool[:len(pool)-1]
	}
}

func listClients() {
	fmt.Println("Clients currently running on Ports:")
	for _, c := range pool {
		fmt.Print(c.port, " ")
	}
	fmt.Println()
}

/*
func cleanupClients() {
	for i, c := range pool {
		select {
		case e := <-c.closed:
			c.closed <- e
			pool[i] = nil
		default:
		}
	}
}
*/
