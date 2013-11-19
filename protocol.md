# Protocol definition

**Basics**:

| 1 Byte       | 1 - 8 Bytes     | 1+ Bytes | 1 - 8 Bytes      | 1+ Bytes |
| :----------: | :-------------: | :------: | :--------------: | :------: |
| Code & Flags | Topic(s) Length | Topic(s) | Message Length   | Message  |

**Additional Fields**:

| 8 or 16 Bytes |
| :----------:  |
| IP Address    |

## Topic
Only Characters above 32 are allowed (UTF is fine).
Multiple Topics can be separated by a null byte. (Any better ideas?)

Topics must not be longer than 128 characters (independent of bytes).

IP Addresses are Either IPv4 or IPv6. Length field before every IP address
needed.

## Flags
| 7     | 6 - 3  | 2   | 1        | 0        |
| :---: | :----: | :-: | :------: | :------: |
| Admin | Action | Zip | Reserved | Reserved |


### Commands Client → Server
**Regular**:

| Binary | Command            | Sections          | Comments                             |
| :---:  | :---               | :---              | :---                                 |
| `0000` | Join               | Topic(s)          | Join / create a topic                |
| `0001` | Part               | Topic(s)          | Unsubscribe from topic               |
| `0010` | Ask for Topic List | Nothing           | Requst topic list                    |
| `0011` | Receive Topic List | Topic(s)          | Receive List of Topics               |
| `0100` | Message            | Topic(s), Message | Send message to topic                |
| `0101` | Binary             | Topic(s), Message | Binary stream, big files             |
| `0110` | Broadcast          | Message           | Send message to all available topics |

**Admin**:

| Binary | Command      | Sections | Comments                            |
| :---:  | :---         | :---     | :---                                |
| `0000` | Close        | Nothing  | Shut down server                    |
| `0001` | Delete topic | Topic    | Unregister everyone from this topic |
| `0010` | Kick user    | User(s)  | Kick User(s), specified by IPs      |
| `0100` | Statistics   | Nothing  | Get statistics for this server      |

### Compression (Zip)
Compression algorithm is [LZ4](https://code.google.com/p/lz4/).
- [Haskell implementation](http://hackage.haskell.org/package/lz4-0.2.2)
- [Golang implementation](https://github.com/salviati/go-lz4)

Messages (or any text really) should only be compressed on message sizes > 20
bytes.  In fact, LZ4 has a hard minimum of 12 bytes if I recall correctly.

### Length Fields
Variable from 1-5 bytes.

Basically: Treat first byte special. Use first 3 bits as length marker for the
length field, the remaining 6 bits as regular number information.

- `000. ....`: 5 bit number
- `10.. ....`: 5 bit number followed by 2 bytes of number information
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

## Discussion
Do you think a nickname system would add something? maybe make it optional? It
would probably take some effort to implement correctly (while still staying 100%
true and backwards compatible to the original specification)
