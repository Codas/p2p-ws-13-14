# Protocol definition

**SplitEdge - MergeEdge - Redirect**

| 1 Byte       | 4 Bytes              | 2 Bytes  | 1 Byte            |
| :----------: | :------------------: | :------: | :---------------: |
| Code & Flags | Address (IPv4)       | Port     | Location          |

**Hello - Cancel - Try-Later**

| 1 Byte       | 1 Byte            | 1 Byte            |
| :----------: | :---------------: | :---------------: |
| Code & Flags | Source Location   | Target Location   |


**ContentMessage**:

| 1 Byte       | 6 Bytes            | 1 Byte               | 1 - 8 Bytes | 1+ Bytes   |
| :----------: | :----------------: | :------------------: | :---------: | :--------: |
| Code & Flags | NodeID (UUID)      | Location (Initiator) | Length      | Message    |

**FishMessage**:
| 1 Byte       | 4 Byte               | 4 Byte               |
| :----------: | :------------------: | :------------------: |
| Code & Flags | Water (32bit Double) | Fish (32bit Double)  |

**Join**:
| 1 Byte       | 6 Bytes            |  
| :----------: | :----------------: | 
| Code & Flags | NewNode IPv4       | 

**RandomWalk**:

| 1 Byte       | 6 Bytes            | 1 Byte               | 
| :----------: | :----------------: | :------------------: |
| Code & Flags | NewNode IPv4       | Hops                 |

## Flags
| 7 - 3  | 2   | 1        | 0        |
| :----: | :-: | :------: | :------: |
| Action | Zip | Reserved | Reserved |


**Action**:

| Binary      | Command    | Comments                           |
| :---:       | :---        | :---                               |
| `00000` (0) | SplitEdge   | Initial Join Request               |
| `00001` (1) | MergeEdge   | Leave Request                      |
| `00010` (2) | Redirect    | Message to change Edge-Destination |
| `00011` (3) | Hello CW    | Ack                                |
| `00100` (4) | Hello CCW   | Ack                                |
| `00101` (5) | Try-Later   | Cancel for MergeEdge               |
| `00110` (6) | Cancel      | Cancel for Redirect                |
| `01000` (8) | Message     |                                    |
| `01001` (9) | FishMessage | Send Water and Fishinformation     |
| `01010` (10)| Join        | Request to find position to join   |
| `01011` (11)| RandomWalk  | Walk 3 log (n) Steps               |


### Compression (Zip)
Compression algorithm is [LZ4](https://code.google.com/p/lz4/).
- [Haskell implementation](http://hackage.haskell.org/package/lz4-0.2.2)
- [Golang implementation](https://github.com/salviati/go-lz4)

Messages (or any text really) should only be compressed on message sizes > 20
bytes.  In fact, LZ4 has a hard minimum of 12 bytes if I recall correctly.

### Length Fields
Variable from 1-7 bytes.

Basically: Treat  first byte special. Use first 3 bits as length marker for the
length field, the remaining 5 bits as regular number information.

- `000. ....`: 5 bit number
- `010. ....`: 5 bit number followed by 2 bytes of number information
  (total 21 bit number)
- `111. ....`: 5 bit number followed by 7 bytes of number information
  (total 61 bit number)

This gives us multiple PT of possible message size.

To be clear: The first 3 bits indicate the length of the length field!
The remaining bits and bites are read like a normal, unsigned integer.

**Example**:

`0010 1001 | 0010 0110 | .... ....` is the number `0 1001 0010 0110` converted to decimal
which is 2342. This means that the directly following message field is exactly
2342 bytes long. The `....` are the first message bits

## General
- All text has to be encoded in UTF-8
