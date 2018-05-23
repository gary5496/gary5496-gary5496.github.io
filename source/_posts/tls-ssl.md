---
title: TLS Handshake
tags: [Network,Security]
date: 2018-03-22 12:32:34
categories: Network
---
TLS/SSL is used to establish connection in HTTPS. Here I will note down my understanding for the TLS handshake process.

The Transport Layer Security (TLS) Handshake Protocol is responsible for the authentication and key exchange necessary to establish or resume secure sessions. When establishing a secure session, the Handshake Protocol manages the following:
- Cipher suite negotiation
- Authentication of the server and optionally, the client
- Session key information exchange.

### Cipher Suite Negotiation
The client and server make contact and choose the *cipher suite* that will be used throughout their message exchange.

### Authentication
In TLS, a server proves its identity to the client. The client might also need to prove its identity to the server. PKI, the use of public/private key pairs, is the basis of this authentication. The exact method used for authentication is determined by the cipher suite negotiated.

### Key Exchange
The client and server exchange *random numbers* and a special number called the *premaster secret*. These numbers are combined with additional data permitting client and server to create their shared secret, called the *master secret*. The *master secret* is used by client and server to generate the *write MAC secret*, which is the session key used for hashing, and the *write key*, which is the session key used for encryption.

### Establishing a Secure Session by Using TLS
1. The client sends a `Client hello` message to the server, along with the *client's random value* and *supported cipher suites*.
2. The server responds by sending a `Server hello` message to the client, along with the *server's random value*, the *selected cipher suite* and *session id*.
3. The server sends its *certificate* to the client with a `Certificate` message for authentication.
4. *Optional.* This step is required when the client requires additional data to generate a *premaster secret* based on the _key exchange algorithm_ such as *DHE_RSA* in the _selected cipher suite_. The server sends a `Server key exchange` message to the client, along with *Diffie-Hellman parameters*.
5. *Optional.* This step is required for client authentication. The server requests a certificate from the client with a `Certificate request` message.
6. The server sends the `Server hello done` message.
7. The client verifies the sever's certificate.
8. *Optional.* This step is required for client authentication. The client sends its *certificate* with a `Certificate` message.
9. The client sends a `Client key exchange` message to the server.
   - It may contain the RSA-encrypted random *premaster secret* based on the _key exchange algorithm_ such as _RSA_ in the _selected cipher suite_. In this case, *premaster secret* is encrypted with the public key from the server's certificate.
   - It may contain *Diffie-Hellman parameters* that will allow each side to agree upon the same *premaster secret* based on the _key exchange algorithm_ such as *DHE_RSA* in the _selected cipher suite_.
10. *Optional.* The client sends a `Certificate verify` message to server.
11. *Optional.* This step is required for client authentication. The server verifies the client's certificate.
12. The server receives the *premaster secret* by decryption or calculation. The server and client each generate the *master secret* and *session keys* based on the *premaster secret*, *client's random value* and *server's random value*.
   - In case of _key exchange algorithm_ such as _RSA_ in the _selected cipher suite_, the server will decrypt the encrypted *premaster secret* with its private key based on RSA asymmetric encryption algorithm.
   - In case of _key exchange algorithm_ such as *DHE_RSA* in the _selected cipher suite_, the server will calculate the *premaster secret* with the *Diffie-Hellman parameters* based on Diffie–Hellman key exchange algorithm.
13. The client sends `Change cipher spec` message to server to indicate that the client will start using the *session key* for hashing and encrypting messages. 
14. Client also sends `Client finished` message which is encrypted with *session key* based on the symmetric encryption algorithm of the _selected cipher suite_.
15. Server sends `Change cipher spec` and switches its record layer security state to symmetric encryption using the *session key*. 
16. Server sends `Server finished` message to the client which is encrypted with *session key* based on the symmetric encryption algorithm of the _selected cipher suite_.
17. Client and server can now exchange application data over the secured channel they have established. All messages sent from client to server and from server to client are encrypted using *session key*.

### Resuming a Secure Session by Using TLS
1. The client sends a `Client hello` message using the *Session ID* of the session to be resumed.
2. The server checks its session cache for a matching Session ID. If a match is found, and the server is able to resume the session, it sends a `Server hello` message with the *Session ID*.
> Note  If a session ID match is not found, the server generates a new session ID and the TLS client and server perform a full handshake.
3. Client and server must exchange `Change cipher spec` messages and send `Client finished` and `Server finished` messages.
4. Client and server can now resume application data exchange over the secure channel.

### Encryption techniques in TLS
TLS uses asymmetric encryption techniques to generate a shared secret key *premaster secret*, which avoids the key distribution problem. It uses the shared key for the symmetric encryption of application data, which is faster than asymmetric encryption.
- Asymmetric encryption: There is a public-private key pair. The data can be encypted with one key, and decrypted with another key.
- Symmetric encryption: There is one shared key. The data can be encypted and decrypted with the same key. It is faster than asymmetric encryption.

### RSA
[RSA](https://en.wikipedia.org/wiki/RSA_%28cryptosystem%29) is an asymmetric public-private key encyption algorithm. In TLS, it is used by client to transfer the *premaster secret* and server to decrypt it to get it.

### AES/3DES 
[AES](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard) and [3DES](https://en.wikipedia.org/wiki/Triple_DES) are symmetric key encyption algorithms. In TLS, it is used by client and server to encypt and decrypt application data.

### Diffie–Hellman key exchange
[Diffie–Hellman](https://en.wikipedia.org/wiki/Diffie%E2%80%93Hellman_key_exchange) is a key exchange algorithm to establish a shared secret between two parties. In TLS, it can be used together than *RSA* to establish a shared secret *premaster secret* between server and client. 

### Cipher Suite
It contains the key exchange algorithm such as RSA/DHE_RSA, and cipher which defines the symmetric encyption algorithm such as AES.

### Reference
[TLS Handshake Protocol (Windows) - MSDN - Microsoft](https://msdn.microsoft.com/en-us/library/windows/desktop/aa380513%28v=vs.85%29.aspx)
[An overview of the SSL or TLS handshake - IBM](https://www.ibm.com/support/knowledgecenter/en/SSFKSJ_7.1.0/com.ibm.mq.doc/sy10660_.htm)
[Transport Layer Security - Wikipedia](https://en.wikipedia.org/wiki/Transport_Layer_Security)
[SSL: Foundation for Web Security - The Internet Protocol](https://www.cisco.com/c/en/us/about/press/internet-protocol-journal/back-issues/table-contents-18/ssl.html)
[RFC 5246 - The Transport Layer Security (TLS) Protocol Version 1.2](https://tools.ietf.org/html/rfc5246)