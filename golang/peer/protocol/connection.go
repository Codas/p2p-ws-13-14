package protocol

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
)

type Connection struct {
	c         net.Conn
	mCallback func(*Connection, *Message)
	cCallback func(*Connection)
}

func NewConnection(c net.Conn, msg func(*Connection, *Message), close func(*Connection)) *Connection {
	conn := &Connection{
		c:         c,
		mCallback: msg,
		cCallback: close,
	}
	if msg != nil {
		go readMessages(conn)
	}

	return conn
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
	case ActionBroadcast:
		buf = append(buf, unparseAddress(msg.Addr)...)
		buf = append(buf, unparseLocation(msg.Loc)...)
		buf = append(buf, unparseContent(msg.Content)...)
	}
	return writeN(c.c, buf)
}

func (c *Connection) SetCallbacks(msg func(*Connection, *Message), close func(*Connection)) {
	if c.mCallback == nil {
		c.mCallback = msg
		c.cCallback = close
		go readMessages(c)
		return
	}
	c.mCallback = msg
	c.cCallback = close
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
		if err == io.EOF {
			if c.cCallback != nil {
				c.cCallback(c)
			}
			return
		} else if err != nil {
			if c.cCallback != nil {
				c.cCallback(c)
			} else {
				fmt.Fprintln(os.Stderr, "CRAZY: close handler is nil")
			}
			//fmt.Fprintln(os.Stderr, "Error reading from socket1: ", err)
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
			c.mCallback(c, &Message{
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
			c.mCallback(c, &Message{
				Action: action,
				Addr:   addr,
				SrcLoc: srcLoc,
				DstLoc: dstLoc,
			})
		case ActionCancel, ActionTryLater, ActionShutdown:
			c.mCallback(c, &Message{
				Action: action,
			})
		case ActionBroadcast:
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
			c.mCallback(c, &Message{
				Action:  action,
				Addr:    addr,
				Loc:     loc,
				Content: content,
			})
		}
	}
}
