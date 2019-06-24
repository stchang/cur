#lang racket/base
(require crypto crypto/libcrypto)
(define rsa-impl (get-pk 'rsa libcrypto-factory))
(define privkey (generate-private-key rsa-impl '((nbits 512))))
(define pubkey (pk-key->public-only-key privkey))
(define privkey2 (generate-private-key rsa-impl '((nbits 512))))
(define pubkey2 (pk-key->public-only-key privkey2))
(provide privkey pubkey privkey2 pubkey2)
