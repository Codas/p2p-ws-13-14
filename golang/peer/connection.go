package main

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
)

type MessageCallbackFunc func(*Connection, *Message)
type ConnectionCloseCallbackFunc func(*Connection)

type Connection struct {
	c         net.Conn
	messageCB MessageCallbackFunc
	closeCB   ConnectionCloseCallbackFunc
}

func NewConnection(c net.Conn, messageCB MessageCallbackFunc, closeCB ConnectionCloseCallbackFunc) *Connection {
	conn := &Connection{
		c:         c,
		messageCB: messageCB,
		closeCB:   closeCB,
	}
	if messageCB != nil {
		go readMessages(conn)
	}

	return conn
}

func ConnectTo(addr *Address, messageCB MessageCallbackFunc, closeCB ConnectionCloseCallbackFunc) (*Connection, error) {
	c, err := net.Dial("tcp", addr.String())
	if err != nil {
		return nil, err
	}
	return NewConnection(c, messageCB, closeCB), nil
}

func (c *Connection) Close() error {
	return c.c.Close()
}

func (c *Connection) Remote() string {
	return c.c.RemoteAddr().String()
}

func (c *Connection) SendMessage(msg *Message) error {
	buf := unparseHeader(msg.Action)
	switch msg.Action {
	case ActionSplitEdge, ActionMergeEdge, ActionRedirect:
		buf = append(buf, unparseAddress(msg.Addr)...)
		buf = append(buf, unparseLocation(msg.Loc)...)
	case ActionHelloCW, ActionHelloCCW:
		buf = append(buf, unparseAddress(msg.Addr)...)
		buf = append(buf, unparseLocation(msg.SrcLoc)...)
		buf = append(buf, unparseLocation(msg.DstLoc)...)
	case ActionBroadcast, ActionGraph:
		buf = append(buf, unparseAddress(msg.Addr)...)
		buf = append(buf, unparseLocation(msg.Loc)...)
		buf = append(buf, unparseContent(msg.Content)...)
	case ActionFish:
		buf = append(buf, unparseFishAndWater(msg.Fish, msg.Water)...)
	case ActionJoin:
		buf = append(buf, unparseAddress(msg.Addr)...)
	case ActionRandomWalk:
		buf = append(buf, unparseAddress(msg.Addr)...)
		buf = append(buf, unparseHops(msg.Hops)...)
	}

	return writeN(c.c, buf)
}

func (c *Connection) SetCallbacks(messageCB MessageCallbackFunc, closeCB ConnectionCloseCallbackFunc) {
	if c.messageCB == nil {
		c.messageCB = messageCB
		c.closeCB = closeCB
		go readMessages(c)
		return
	}
	c.messageCB = messageCB
	c.closeCB = closeCB
}

func writeN(w io.Writer, buf []byte) error {
	if n, err := w.Write(buf); err != nil {
		return err
	} else if n != len(buf) {
		return errors.New("Write length missmatch!")
	}
	return nil
}

func readMessages(c *Connection) {
	br := bufio.NewReader(c.c)
	for {
		action, err := parseHeader(br)
		if err != nil {
			if c.closeCB != nil {
				c.closeCB(c)
			}
			return
		}
		switch action {
		case ActionSplitEdge, ActionMergeEdge, ActionRedirect:
			addr, err := parseAddress(br)
			if err != nil {
				fmt.Fprintln(os.Stderr, "Error reading from socket2: ", err)
				return
			}
			loc, err := parseLocation(br)
			if err != nil {
				fmt.Fprintln(os.Stderr, "Error reading from socket3: ", err)
				return
			}
			c.messageCB(c, &Message{
				Action: action,
				Addr:   addr,
				Loc:    loc,
			})
		case ActionHelloCW, ActionHelloCCW:
			addr, err := parseAddress(br)
			if err != nil {
				fmt.Fprintln(os.Stderr, "Error reading from socket4: ", err)
				return
			}
			srcLoc, err := parseLocation(br)
			if err != nil {
				fmt.Fprintln(os.Stderr, "Error reading from socket5: ", err)
				return
			}
			dstLoc, err := parseLocation(br)
			if err != nil {
				fmt.Fprintln(os.Stderr, "Error reading from socket6: ", err)
				return
			}
			c.messageCB(c, &Message{
				Action: action,
				Addr:   addr,
				SrcLoc: srcLoc,
				DstLoc: dstLoc,
			})
		case ActionCancel, ActionTryLater, ActionShutdown, ActionJoin:
			c.messageCB(c, &Message{
				Action: action,
			})
		case ActionBroadcast, ActionGraph:
			addr, err := parseAddress(br)
			if err != nil {
				fmt.Fprintln(os.Stderr, "Error reading from socket2: ", err)
				return
			}
			loc, err := parseLocation(br)
			if err != nil {
				fmt.Fprintln(os.Stderr, "Error reading from socket3: ", err)
				return
			}
			content, err := parseContent(br)
			if err != nil {
				fmt.Fprintln(os.Stderr, "Error reading from socket7: ", err)
				return
			}
			c.messageCB(c, &Message{
				Action:  action,
				Addr:    addr,
				Loc:     loc,
				Content: content,
			})
		case ActionFish:
			fish, water, err := parseFishAndWater(br)
			if err != nil {
				fmt.Fprintln(os.Stderr, "Error on parsing fish and water", err)
			}
			c.messageCB(c, &Message{
				Action: action,
				Fish:   fish,
				Water:  water,
			})
		case ActionRandomWalk:
			addr, err := parseAddress(br)
			if err != nil {
				fmt.Fprintln(os.Stderr, "Error on parsing Addr of RandomWalk", err)
			}
			hops, err := parseHops(br)
			if err != nil {
				fmt.Fprintln(os.Stderr, "Error on parsing hops of RandomWalk", err)
			}
			c.messageCB(c, &Message{
				Action: action,
				Addr:   addr,
				Hops:   hops,
			})
		}
	}
}
