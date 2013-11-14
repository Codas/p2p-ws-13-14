# Protocol definition

| 1 Byte       | 1 - 8 Bytes   | 1+ Bytes | 1 - 8 Bytes      | 1+ Bytes |
| :----------: | :-----------: | :------: | :--------------: | :------: |
| Code & Flags | Topic Length  | Topic    | Message Length   | Message  |

## Flags
| 7     | 6 - 3  | 2   | 1        | 0        |
| :---: | :----: | :-: | :------: | :------: |
| Admin | Action | Zip | Reserved | Reserved |


### Commands
**Regular**:

| Binary | Command      | Sections       | Comments                                       |
| :---:  | :---         | :---           | :---                                           |
| `0001` | join         | Topic only     | Join / create a topic                          |
| `0010` | part         | Topic only     | Unsubscribe from topic                         |
| `0100` | Send Message | Topic, Message | Send message to topic                          |
| `0101` | Send Binary  | Topic, Message | Binary stream, big files                       |
| `0110` | Topic List   | Nothing        | Requst topic list                              |
| `0111` | Topic Info   | Topic          | Topic Information, e.g. user count, list, etc. |

Topic Info and Topic List need further design specification (response protocol?)

**Admin**:

???

### Compression (Zip)
Compression algorithm is [LZ4](https://code.google.com/p/lz4/).
- [Haskell implementation](http://hackage.haskell.org/package/lz4-0.2.2)
- [Golang implementation](https://github.com/salviati/go-lz4)

Messages (or any text really) should only be compressed on message sizes > 20 bytes.
In fact, LZ4 has a hard minimum of 12 bytes if I recall correctly.

### Length Fields
Variable from 1-8 bytes, somewhat like UTF-8 does it.

Basically: Treat first byte special. Split into length marker followed by `0`,
followed by the actual number bits.

- `0.......`: 7 bit number
- `110.....`: 5 bit number followed by 2 bytes of number information
  (total 21 bit number)
- `11111110`: 0 bit number followed by 7 bytes of number information
  (total 56 bit number)

I know this is not exactly how UTF-8 does it, but I do not think that we need
the firsts bit to be a continuation bits (in UTF-8 for ascii
compatibility). This gives us a maximum message size of multiple PB.


**Alternative**:

Always use first 2 or 3 bits as number overflow length, rest as number
bits. That would probably simplify implementation and still leave us with 256GB
(2 bit length) or multiple PB (3 bit length) of allowed message size.

Downside would be to almost always use 2 bytes for the length field (message
length of 16 to 32 byte possible with one length byte). Starting with the 2nd
byte however `10.. .... .... ....` for variable with length information vs
`001. .... .... ....` for the 3 bit fixed length implementation, any remaining
differences become insignificant.

## General
- All text has to be encoded in UTF-8

## Discussion
Do you think a nickname system would add something? maybe make it optional? It
would probably take some effort to implement correctly (while still staying 100%
true and backwards compatible to the original specification)
