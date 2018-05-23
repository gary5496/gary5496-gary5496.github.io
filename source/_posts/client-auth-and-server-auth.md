---
title: SSL Client Authentication and SSL Server Authentication
tags: [Network,Security]
date: 2018-03-22 22:12:01
categories: Network
---

### Messages Exchanged During SSL Handshake

The following steps describes the sequence of messages exchanged during an SSL handshake. These step describe the programmatic details of the messages exchanged during the SSL handshake.

1. The client sends the server the client’s SSL version number, cipher settings, randomly generated data, and other information the server needs to communicate with the client using SSL. `Client hello`

2. The server sends the client the server’s SSL version number, cipher settings, randomly generated data, and other information the client needs to communicate with the server over SSL. The server also sends its own certificate and, if the client is requesting a server resource that requires client authentication, requests the client’s certificate. `Server hello` `Certificate` `Certificate request`

3. The client can use some of the information sent by the server to authenticate the server. For details, see `Server Authentication During SSL Handshake`. If the server cannot be authenticated, the user is warned of the problem and informed that an encrypted and authenticated connection cannot be established. If the server can be successfully authenticated, the client goes on to Step 4.

4. Using all data generated in the handshake so far, the client, with the cooperation of the server, depending on the cipher being used, creates the pre-master secret for the session, encrypts it with the server’s public key, obtained from the server’s certificate, sent in Step 2, and sends the encrypted pre-master secret to the server. `Client key exchange`

5. If the server has requested client authentication (an optional step in the handshake), the client also signs another piece of data that is unique to this handshake and known by both the client and server. In this case the client sends both the signed data and the client’s own certificate to the server along with the encrypted pre-master secret. `Certificate` `Client key exchange`

6. If the server has requested client authentication, the server attempts to authenticate the client. For details, see `Client Authentication During SSL Handshake`. If the client cannot be authenticated, the session is terminated. If the client can be successfully authenticated, the server uses its private key to decrypt the pre-master secret, then performs a series of steps (which the client also performs, starting from the same pre-master secret) to generate the master secret.

7. Both the client and the server use the master secret to generate the session keys, which are symmetric keys used to encrypt and decrypt information exchanged during the SSL session and to verify its integrity—that is, to detect changes in the data between the time it was sent and the time it is received over the SSL connection.

8. The client sends a message to the server informing it that future messages from the client are encrypted with the session key. It then sends a separate (encrypted) message indicating that the client portion of the handshake is finished. `Change cipher spec` `Client finished`

9. The server sends a message to the client informing it that future messages from the server are encrypted with the session key. It then sends a separate (encrypted) message indicating that the server portion of the handshake is finished. `Change cipher spec` `Server finished`

10. The SSL handshake is now complete, and the SSL session has begun. The client and the server use the session keys to encrypt and decrypt the data they send to each other and to validate its integrity.

Before continuing with a session, directory servers can be configured to check that the client’s certificate is present in the user’s entry in an LDAP directory. This configuration option provides one way of ensuring that the client’s certificate has not been revoked.

Both client and server authentication involve encrypting some piece of data with one key of a public-private key pair and decrypting it with the other key:

- In the case of server authentication, the client encrypts the pre-master secret with the server’s public key. Only the corresponding private key can correctly decrypt the secret, so the client has some assurance that the identity associated with the public key is in fact the server with which the client is connected. Otherwise, the server cannot decrypt the pre-master secret and cannot generate the symmetric keys required for the session, and the session is terminated.

- In the case of client authentication, the client encrypts some random data with the client’s private key—that is, it creates a digital signature. The public key in the client’s certificate can correctly validate the digital signature only if the corresponding private key was used. Otherwise, the server cannot validate the digital signature and the session is terminated.

### Server Authentication During SSL Handshake

SSL-enabled client software always requires server authentication, or cryptographic validation by a client of the server’s identity. The server sends the client a certificate to authenticate itself. The client uses the certificate to authenticate the identity the certificate claims to represent.

To authenticate the binding between a public key and the server identified by the certificate that contains the public key, an SSL-enabled client must receive a yes answer to the four questions shown in the following figure.

*Figure 5–9 Authenticating a Server Certificate During SSL Handshake*
![Server authentication](https://docs.oracle.com/cd/E19424-01/820-4811/images/svrauth.gif)

An SSL-enabled client goes through the following steps to authenticate a server’s identity:

1. Is today’s date within the validity period?
   
   The client checks the server certificate’s validity period. If the current date and time are outside of that range, the authentication process won’t go any further. If the current date and time are within the certificate’s validity period, the client goes on to the next step.

2. Is the issuing CA a trusted CA?
   
   Each SSL-enabled client maintains a list of trusted CA certificates, represented by the shaded area on the right—hand side of Figure 5–9. This list determines which server certificates the client accepts. If the distinguished name (DN) of the issuing CA matches the DN of a CA on the client’s list of trusted CAs, the answer to this question is yes, and the client goes on to the next step. If the issuing CA is not on the list, the server is not authenticated unless the client can verify a certificate chain ending in a CA that is on the list.

3. Does the issuing CA’s public key validate the issuer’s digital signature?
   
   The client uses the public key from the CA’s certificate (which it found in its list of trusted CAs in step 2) to validate the CA’s digital signature on the server certificate being presented. If the information in the server certificate has changed since it was signed by the CA or if the CA certificate’s public key doesn’t correspond to the private key used by the CA to sign the server certificate, the client won’t authenticate the server’s identity. If the CA’s digital signature can be validated, the server treats the user’s certificate as a valid “letter of introduction” from that CA and proceeds. At this point, the client has determined that the server certificate is valid.

4. Does the domain name in the server’s certificate match the domain name of the server itself?
   
   This step confirms that the server is actually located at the same network address specified by the domain name in the server certificate. Although step 4 is not technically part of the SSL protocol, it provides the only protection against a form of security attack known as man-in-the-middle. Clients must perform this step and must refuse to authenticate the server or establish a connection if the domain names don’t match. If the server’s actual domain name matches the domain name in the server certificate, the client goes on to the next step.

5. The server is authenticated.

   The client proceeds with the SSL handshake. If the client doesn’t get to step 5 for any reason, the server identified by the certificate cannot be authenticated, and the user is warned of the problem and informed that an encrypted and authenticated connection cannot be established. If the server requires client authentication, the server performs the steps described in `Client Authentication During SSL Handshake`.

After the steps described here, the server must successfully use its private key to decrypt the pre-master secret sent by the client.

### Client Authentication During SSL Handshake

SSL-enabled servers can be configured to require client authentication, or cryptographic validation by the server of the client’s identity. When a server configured this way requests client authentication separate piece of digitally signed data to authenticate itself. The server uses the digitally signed data to validate the public key in the certificate and to authenticate the identity the certificate claims to represent.

The SSL protocol requires the client to create a digital signature by creating a one-way hash from data generated randomly during the handshake and known only to the client and server. The hash of the data is then encrypted with the private key that corresponds to the public key in the certificate being presented to the server.

To authenticate the binding between the public key and the person or other entity identified by the certificate that contains the public key, an SSL-enabled server must receive a yes answer to the first four questions shown in Figure 5–10. Although the fifth question is not part of the SSL protocol, directory servers can be configured to support this requirement to take advantage of the user entry in an LDAP directory as part of the authentication process.

*Figure 5–10 Authentication and Verification During SSL Handshake*
![Client authentication](https://docs.oracle.com/cd/E19424-01/820-4811/images/cert.gif)

An SSL-enabled server goes through the following steps to authenticate a user’s identity:

1. Does the user’s public key validate the user’s digital signature?

  The server checks that the user’s digital signature can be validated with the public key in the certificate. If so, the server has established that the public key asserted to belong to John Doe matches the private key used to create the signature and that the data has not been tampered with since it was signed.

  At this point, however, the binding between the public key and the DN specified in the certificate has not yet been established. The certificate might have been created by someone attempting to impersonate the user. To validate the binding between the public key and the DN, the server must also complete steps 3 and 4 in this list.

2. Is today’s date within the validity period?

  The server checks the certificate’s validity period. If the current date and time are outside of that range, the authentication process won’t go any further. If the current date and time are within the certificate’s validity period, the server goes onto the next step.

3. Is the issuing CA a trusted CA?

  Each SSL-enabled server maintains a list of trusted CA certificates, represented by the shaded area on the right—hand side of Figure 5–10. This list determines which certificates the server accepts. If the DN of the issuing CA matches the DN of a CA on the server’s list of trusted CAs, the answer to this question is yes, and the server goes on to the next step. If the issuing CA is not on the list, the client is not authenticated unless the server can verify a certificate chain ending in a CA that is trusted or not trusted within their organizations by controlling the lists of CA certificates maintained by clients and servers.

4. Does the issuing CA’s public key validate the issuer’s digital signature?

  The server uses the public key from the CA’s certificate (which it found in its list of trusted CAs in the previous step) to validate the CA’s digital signature on the certificate being presented. If the information in the certificate has changed since it was signed by the CA or if the public key in the CA certificate doesn’t correspond to the private key used by the CA to sign the certificate, the server won’t authenticate the user’s identity. If the CA’s digital signature can be validated, the server treats the user’s certificate as a valid “letter of introduction” from that CA and proceeds. At this point, the SSL protocol allows the server to consider the client authenticated and proceed with the connection as described in step 6. The directory servers may optionally be configured to perform step 5 before step 6.

5. Is the user’s certificate listed in the LDAP entry for the user?

  This optional step provides one way for a system administrator to revoke a user’s certificate even if it passes the tests in all the other steps. The Certificate Management System can automatically remove a revoked certificate from the user’s entry in the LDAP directory. All servers that are set up to perform this step then refuses to authenticate that certificate or establish a connection. If the user’s certificate in the directory is identical to the user’s certificate presented in the SSL handshake, the server goes on to the next step.

6. Is the authenticated client authorized to access the requested resources?

  The server checks what resources the client is permitted to access according to the server’s access control lists (ACLs) and establishes a connection with appropriate access. If the server doesn’t get to step 6 for any reason, the user identified by the certificate cannot be authenticated, and the user is not allowed to access any server resources that require authentication.


### Digital Signatures
Digital signatures can be used by Directory Server to maintain integrity of information. If encryption and message digests are applied to the information being sent, the recipient can determine that the information was not tampered with during transit.

Tamper detection and related authentication techniques rely on a mathematical function called a one-way hash. This function is also called a message digest. A one-way hash is a number of fixed length with the following characteristics:

- The value of the hash is unique for the hashed data. Any change in the data, even deleting or altering a single character, results in a different value.

- The content of the hashed data cannot, for all practical purposes, be deduced from the hash — which is why it is called *one-way*.

It is possible to use a private key for encryption and a public key for decryption. Although this is not desirable when you are encrypting sensitive information, it is a crucial part of digitally signing any data. Instead of encrypting the data itself, the signing software creates a one-way hash of the data, then uses your private key to encrypt the hash. The encrypted hash, along with other information, such as the hashing algorithm, is known as a digital signature. Figure 5–11 shows two items transferred to the recipient of some signed data.

*Figure 5–11 Digital Signatures*
![Digital Signature](https://docs.oracle.com/cd/E19424-01/820-4811/images/digsgn.gif)

In Figure 5–11, the original data and the digital signature, which is basically a one-way hash (of the original data) that has been encrypted with the signer's private key. To validate the integrity of the data, the receiving software first uses the signer’s public key to decrypt the hash. It then uses the same hashing algorithm that generated the original hash to generate a new one-way hash of the same data. (Information about the hashing algorithm used is sent with the digital signature, although this isn’t shown in the figure.) Finally, the receiving software compares the new hash against the original hash. If the two hashes match, the data has not changed since it was signed. If they don’t match, the data may have been tampered with since it was signed, or the signature may have been created with a private key that doesn’t correspond to the public key presented by the signer.

If the two hashes match, the recipient can be certain that the public key used to decrypt the digital signature corresponds to the private key used to create the digital signature. Confirming the identity of the signer, however, also requires some way of confirming that the public key really belongs to a particular person or other entity.

The significance of a digital signature is comparable to the significance of a handwritten signature. Once you have signed some data, it is difficult to deny doing so later — assuming that the private key has not been compromised or out of the owner’s control. This quality of digital signatures provides a high degree of non-repudiation — that is, digital signatures make it difficult for the signer to deny having signed the data. In some situations, a digital signature may be as legally binding as a handwritten signature.

### Reference

[Messages Exchanged During SSL Handshake](https://docs.oracle.com/cd/E19424-01/820-4811/gbfqu/index.html)
[Server Authentication During SSL Handshake](https://docs.oracle.com/cd/E19424-01/820-4811/aakhc/index.html)
[Client Authentication During SSL Handshake](https://docs.oracle.com/cd/E19424-01/820-4811/aakhe/index.html)
[Digital Signatures](https://docs.oracle.com/cd/E19424-01/820-4811/aakfx/index.html)