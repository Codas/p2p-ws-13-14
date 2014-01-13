package main

import (
	"bufio"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"strings"
	"time"
)

var (
	port      = flag.Int("p", 1337, "Port at which to listen")
	verbosity = flag.Int("v", 2, "verbosity level [0=none, 1=only peer, 2=also nodes]")
)

func main() {
	flag.Parse()

	p := NewPeer(*port, *verbosity, graphCallback)

	consoleLoop(p)

	p.Shutdown()
}

func consoleLoop(p *Peer) {
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
		case "v":
			setVerbosityLevel(p, args)
		case "l":
			p.ListNodes()
		case "cycle":
			p.AddNode(nil)
		case "check":
			p.CheckNodes()
		case "c":
			connectNewNode(p, args)
		case "cm":
			connectMany(p, args)
		case "d":
			disconnectNode(p, args)
		case "da":
			p.DisconnectAllNodes()
		case "b":
			p.Broadcast(args)
		case "g":
			p.GenerateGraph()
		case "gf":
			togglePeriodicGraphFile(p, args)
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
	fmt.Println("- v <level> (set verbosity level [2 = all, 1 = only peer])")
	fmt.Println("- l (list all nodes)")
	fmt.Println("- cycle (create a node that points to itself)")
	fmt.Println("- check (check neighbors)")
	fmt.Println("- c <ip:port> (connect new node to <ip:port>)")
	fmt.Println("- cm <num> <ip:port> (connect <num> nodes to <ip:port>)")
	fmt.Println("- d [<#location>] (disconnect node on <#location>)")
	fmt.Println("- da (disconnect all)")
	fmt.Println("- b <text> (broadcast <text>)")
	fmt.Println("- g (generate graph)")
	fmt.Println("- gf [<intervall>] (periodically generate graph to file (in ms))")
}

func setVerbosityLevel(p *Peer, text string) {
	var level int
	if _, err := fmt.Sscan(text, &level); err != nil {
		fmt.Fprintf(os.Stderr, "Error: parameter needs to have form '<level>' (%s)\n", err)
		return
	}
	*verbosity = level
	p.SetVerbosityLevel(*verbosity)
}

func connectNewNode(p *Peer, text string) {
	address, err := ParseAddress(text)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error parsing address: needs to have form ':<port>' / 'localhost:<port>' / '<ipv4>:<port>' (%s)\n", err)
		return
	}

	p.AddNode(address)
}

func connectMany(p *Peer, text string) {
	var num int
	var addresstext string
	if _, err := fmt.Sscanf(text, "%d %s", &num, &addresstext); err != nil {
		fmt.Fprintf(os.Stderr, "Error: parameter needs to have form '<num> <ip:port>' (%s)\n", err)
		return
	}
	address, err := ParseAddress(addresstext)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error parsing address: needs to have form ':<port>' / 'localhost:<port>' / '<ipv4>:<port>' (%s)\n", err)
		return
	}

	for i := 0; i < num; i++ {
		p.AddNode(address)
	}
}

func disconnectNode(p *Peer, text string) {
	loc := Location(255)
	if text != "" {
		if _, err := fmt.Sscan(text, &loc); err != nil {
			fmt.Fprintf(os.Stderr, "Error parsing <#location>: (%s)!\n", err)
			return
		}
	}

	if !p.DisconnectNode(loc) {
		fmt.Fprintf(os.Stderr, "Invalid <#location>!\n")
		return
	}
}

// graph ticker
var ticker *time.Ticker

func togglePeriodicGraphFile(p *Peer, text string) {
	// Stop ticker, if its already running
	if ticker != nil {
		ticker.Stop()
		ticker = nil
		fmt.Println("Deactivated periodic graph query")
		return
	}

	// find out interval
	interval := 5000
	if text != "" {
		if _, err := fmt.Sscan(text, &interval); err != nil {
			fmt.Fprintf(os.Stderr, "Error parsing <interval>: (%s)!\n", err)
			return
		}
	}

	// periodically generate graph
	fmt.Printf("Activating periodic graph query (interval: %d ms)", interval)
	ticker = time.NewTicker(time.Duration(interval) * time.Millisecond)
	go func(t *time.Ticker) {
		for _ = range t.C {
			p.GenerateGraph()
		}
	}(ticker)
}

type jsonEntry struct {
	Id string
}

func writeJSONGraph(g []*NodeAttr) {
	var jG []jsonEntry
	for _, n := range g {
		jG = append(jG, jsonEntry{fmt.Sprintf("%d:%d", n.Addr.Port, n.Loc)})
	}

	data, err := json.Marshal(&jG)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error marshalling JSON: %s\n", err)
		return
	}

	f, err := os.Create("graph.json")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating file: %s\n", err)
		return
	}
	defer f.Close()

	_, err = f.Write(data)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error writing file: %s\n", err)
		return
	}
}

func writeDOTGraphs(g []*NodeAttr) {
	// chained graph
	fG, err := os.Create("graph.dot")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating file: %s\n", err)
		return
	}
	defer fG.Close()

	// multigraph
	fM, err := os.Create("graphMG.dot")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating file: %s\n", err)
		return
	}
	defer fM.Close()

	fG.WriteString("digraph p2p {\n")
	fM.WriteString("digraph p2p {\n")
	for _, n := range g {
		fG.WriteString(fmt.Sprintf("N%d_%d", n.Addr.Port, n.Loc))
		fM.WriteString(fmt.Sprintf("N%d", n.Addr.Port))

		fG.WriteString(" -> ")
		fM.WriteString(" -> ")
	}
	fG.WriteString(fmt.Sprintf("N%d_%d", g[0].Addr.Port, g[0].Loc))
	fM.WriteString(fmt.Sprintf("N%d", g[0].Addr.Port))
	fG.WriteString(";\n}")
	fM.WriteString(";\n}")
}

func graphCallback(g []*NodeAttr) {
	if ticker != nil {
		fmt.Println("Writing graph to file")
		writeJSONGraph(g)
		writeDOTGraphs(g)
		return
	}
	fmt.Println("Graph:")
	for _, n := range g {
		fmt.Printf(" -> (%d:%d)", n.Addr.Port, n.Loc)
	}
	fmt.Print("\n")
}
