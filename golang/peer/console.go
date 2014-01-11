package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"math/rand"
	"os"
	"strconv"
	"strings"
	"time"

	p "../peer/protocol"
)

var r = rand.New(rand.NewSource(time.Now().UnixNano()))
var ticker *time.Ticker

func parseStdIO() {
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
			listNodes()
		case "cycle":
			createCycleNode()
		case "c":
			connectNewNode(args)
		case "cm":
			connectMany(args)
		case "d":
			disconnectNode(args)
		case "da":
			disconnectAllNodes()
		case "b":
			broadcastNode(args)
		case "g":
			graphNode(args)
		case "gf":
			graphFile(args)
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
	fmt.Println("- l (list all peers, shows <#node>)")
	fmt.Println("- cycle (create a node that points to itself)")
	fmt.Println("- c <ip> <port> (connect new node to <ip> <port>)")
	fmt.Println("- cm <num> <ip> <port> (connect many (num))")
	fmt.Println("- d <#node> (disconnect <#node>)")
	fmt.Println("- da (disconnect all)")
	fmt.Println("- b (<#node>) (broadcast on <#node>)")
	fmt.Println("- g (<#node>) (generate graph on <#node>)")
	fmt.Println("- gf (<intervall>) (periodically generate graph to file (in ms))")
	//fmt.Println("- br <#node> <delay> (broadcast periodically on <#node> with <delay>)")
}

func listNodes() {
	m.RLock()
	defer m.RUnlock()

	if len(nodes) == 0 {
		fmt.Println("No nodes currently running")
		return
	}

	for i, n := range nodes {
		fmt.Printf("%d: %s\n", i, n)
	}
}

func graphNode(sIdx string) {
	m.RLock()
	defer m.RUnlock()

	idx := r.Intn(len(nodes))
	if sIdx != "" {
		var err error
		idx, err = strconv.Atoi(sIdx)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Parse Error: %s\n", err)
			return
		}
		if idx < 0 || idx >= len(nodes) {
			fmt.Fprintf(os.Stderr, "Invalid <#node>, should be [0 - %d).\n", len(nodes))
			return
		}
	}

	nodes[idx].InitiateGraph()
}

func graphFile(sInterval string) {
	if ticker != nil {
		ticker.Stop()
		ticker = nil
		fmt.Println("Deactivated periodic graph query")
		return
	}
	intervall := 1000
	if sInterval != "" {
		var err error
		intervall, err = strconv.Atoi(sInterval)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Parse Error: %s\n", err)
			return
		}
	}

	fmt.Printf("Activating periodic graph query (intervall: %d ms)", intervall)
	ticker = time.NewTicker(time.Duration(intervall) * time.Millisecond)
	go func(t *time.Ticker) {
		for _ = range t.C {
			m.RLock()
			if len(nodes) == 0 {
				return
			}

			//idx := r.Intn(len(nodes))
			idx := 0
			nodes[idx].InitiateGraph()
			m.RUnlock()
		}
	}(ticker)
}

type jsonEntry struct {
	Id string
}

func writeJSONGraph(g []*p.NodeAttr) {
	var jG []jsonEntry
	for _, n := range g {
		jG = append(jG, jsonEntry{(strconv.Itoa(n.Addr.Port()) + ":" + strconv.Itoa(int(n.Loc)))})
	}

	data, err := json.Marshal(&jG)
	if err != nil {
		fmt.Fprintf(os.Stderr, "JSON Marshal Error: %s\n", err)
		return
	}

	f, err := os.Create("graph.json")
	if err != nil {
		fmt.Fprintf(os.Stderr, "File Creation Error: %s\n", err)
		return
	}
	defer f.Close()

	_, err = f.Write(data)
	if err != nil {
		fmt.Fprintf(os.Stderr, "File Writing Error: %s\n", err)
		return
	}
}

func graphCallback(g []*p.NodeAttr) {
	if ticker != nil {
		writeJSONGraph(g)
		return
	}
	fmt.Println("Graph:")
	for _, n := range g {
		fmt.Print(" -> (", n.Addr.Port(), ":", n.Loc, ")")
	}
	fmt.Print("\n")
}

func broadcastNode(sIdx string) {
	m.RLock()
	defer m.RUnlock()

	idx := r.Intn(len(nodes))
	if sIdx != "" {
		var err error
		idx, err = strconv.Atoi(sIdx)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Parse Error: %s\n", err)
			return
		}
		if idx < 0 || idx >= len(nodes) {
			fmt.Fprintf(os.Stderr, "Invalid <#node>, should be [0 - %d).\n", len(nodes))
			return
		}
	}

	nodes[idx].InitiateBroadcast()
}

func connectMany(text string) {
	addrparts := strings.Split(text, " ")
	if len(addrparts) != 3 {
		fmt.Fprintln(os.Stderr, "Parameter does not have form 'num ip port'")
		return
	}
	num, err := strconv.Atoi(addrparts[0])
	if err != nil {
		fmt.Fprintf(os.Stderr, "Parsing Error: Num (%s) is not a number\n", addrparts[2])
		return
	}
	port, err := strconv.Atoi(addrparts[2])
	if err != nil {
		fmt.Fprintf(os.Stderr, "Parsing Error: Port (%s) is not a number\n", addrparts[2])
		return
	}
	rAddr := p.NewAddress(addrparts[1], port)

	for i := 0; i < num; i++ {
		n := p.NewNode(localAddress(), rAddr, uniqueLocation(), removeNode, graphCallback)
		if n == nil {
			continue
		}

		m.Lock()
		nodes = append(nodes, n)
		m.Unlock()
	}
}

func connectNewNode(addr string) {
	addrparts := strings.Split(addr, " ")
	if len(addrparts) != 2 {
		fmt.Fprintln(os.Stderr, "Parameter does not have form 'ip port'")
		return
	}
	port, err := strconv.Atoi(addrparts[1])
	if err != nil {
		fmt.Fprintf(os.Stderr, "Parsing Error: Port (%s) is not a number\n", addrparts[1])
		return
	}
	rAddr := p.NewAddress(addrparts[0], port)

	n := p.NewNode(localAddress(), rAddr, uniqueLocation(), removeNode, graphCallback)
	if n == nil {
		return
	}

	m.Lock()
	nodes = append(nodes, n)
	m.Unlock()
}

func disconnectNode(sIdx string) {
	m.RLock()
	defer m.RUnlock()

	idx := r.Intn(len(nodes))
	if sIdx != "" {
		var err error
		idx, err = strconv.Atoi(sIdx)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Parse Error: %s\n", err)
			return
		}
		if idx < 0 || idx >= len(nodes) {
			fmt.Fprintf(os.Stderr, "Invalid <#node>, should be [0 - %d).\n", len(nodes))
			return
		}
	}

	nodes[idx].InitiateMergeEdge()
}
