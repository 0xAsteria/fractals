;; @contract Fractal
;; @version 1.8
;; @author asteria.id

;; all rights reserved (c) 2022 to the above author.
;; this code will be relicensed under a more permissive 
;; license on mainnet launch, by asteria.id

;; asteria.id means the owner of the Stacks mainnet BNS name `asteria.id'
;; as of this writing, the Stacks address of said owner is:
;;   SP343J7DNE122AVCSC4HEK4MF871PW470ZSXJ5K66

(use-trait sip9 .sip9.nft-trait)
(define-fungible-token fractal)


;;              P U B L I C


;; @desc Accepts any SIP-009 NFT and fractalizes it to the tx-sender
;; @param nftContract; A SIP-009 NFT contract address
;; @param nftId; The original uint ID of the NFT used in SIP-009
;; @post tok; The NFT is transferred from the user to this contract

(define-public (fractalize-nft (nftContract <sip9>) (nftId uint) (fractalCount uint))
  (let (
    (owner (unwrap-panic (get-owner nftContract nftId)))
    (userId (get-or-create-user-id tx-sender))
    (targetId (var-get nftTip))
  )
    (begin 
      (asserts! (contract-is-whitelisted nftContract) (err UNAUTHORIZED_CONTRACT))
      (asserts! (is-eq owner tx-sender) (err UNAUTHORIZED))
      (asserts! (> fractalCount u0) (err FRACTAL_COUNT_TOO_LOW))
      (try! (transfer-nft-to-vault nftContract nftId))
      (try! (ft-mint? fractal fractalCount tx-sender))
      (map-insert userBalances { userId: userId, nftId: targetId } fractalCount)
      (map-insert fractalNfts targetId { 
        totalSupply: fractalCount, sip9Contract: (contract-of nftContract), sip9Id: nftId, 
        issuer: tx-sender, uri: (try! (pull-token-uri nftContract nftId)) })
      (unwrap-panic (add-nft-to-owned-tokens userId targetId))
      (var-set nftTip (+ targetId u1))
      (ok targetId)
    )
  )
)

;; @desc Defractalizes an NFT from all of its fractals and releases it to the tx-sender
;; @param nftContract; The SIP-009 NFT contract address
;; @param nftId; The uint ID of the NFT used in SIP-009
;; @param fractalNftId; The uint Fractal ID of the NFT
;; @post tok; The NFT is transferred from this contract to the tx-sender

(define-public (defractalize-nft (nftContract <sip9>) (fractalNftId uint))
  (let (
    (userId (get-or-create-user-id tx-sender))
    (balance (get-or-create-fractal-balance userId fractalNftId))
    (details (unwrap-panic (map-get? fractalNfts fractalNftId )))
  )
    (begin 
      (asserts! (is-eq (contract-of nftContract) (get sip9Contract details))
        (err UNAUTHORIZED_CONTRACT))
      (asserts! (is-eq balance (get totalSupply details)) (err FRACTAL_COUNT_TOO_LOW))
      (try! (ft-burn? fractal balance tx-sender))
      (try! (transfer-nft-out-vault nftContract (get sip9Id details)))
      (map-set fractalNfts fractalNftId { 
        totalSupply: u0, sip9Contract: (get sip9Contract details), 
        sip9Id: (get sip9Id details), issuer: (get issuer details),
        uri: (get uri details)
      })
      (map-delete userBalances { userId: userId, nftId: fractalNftId })
      (remove-nft-from-owned-tokens userId fractalNftId)
      (ok fractalNftId)
    )
  )
)

;; @desc Transfers fractals from tx-sender to another principal
;; @param amount; The amount of fractals to transfer
;; @param recipient; The principal of the user to transfer to
;; @param memo; A memo to attach to the transaction
;; @param nftId; The uint Fractal ID of the NFT

(define-public (transfer 
    (amount uint) 
    (nftId uint)
    (recipient principal) 
    (memo (optional (buff 34))) 
  )
    (begin
      (asserts! (> amount u0) (err FRACTAL_COUNT_TOO_LOW))
      (asserts! (<= amount (unwrap-panic 
        (get-fractal-balance tx-sender nftId))) (err BALANCE_TOO_LOW)
      )
      (try! (ft-transfer? fractal amount tx-sender recipient))

      ;; update balances in maps
      (let (
        (senderUserId (get-or-create-user-id tx-sender))
        (recipientUserId (get-or-create-user-id recipient))

        (senderStartBalance (get-or-create-fractal-balance senderUserId nftId))
        (recipientStartBalance (get-or-create-fractal-balance recipientUserId nftId))

        (senderNewBalance (- senderStartBalance amount))
        (recipientNewBalance (+ recipientStartBalance amount))
      )
        (begin
          (map-set userBalances { userId: senderUserId, nftId: nftId } senderNewBalance)
          (map-set userBalances { userId: recipientUserId, nftId: nftId } recipientNewBalance)
          
          (if (is-eq senderNewBalance u0) 
            (remove-nft-from-owned-tokens senderUserId nftId)
            true
          )

          (unwrap-panic (add-nft-to-owned-tokens recipientUserId nftId))
        )
      )

      (print memo)
      (ok true)
    )
)


;;              E R R O R   C O D E S


(define-constant USER_DOES_NOT_EXIST u300)
(define-constant NFT_DOES_NOT_EXIST u301)
(define-constant USER_OWNED_TOKENS_DOES_NOT_EXIST u302)
(define-constant FRACTAL_COUNT_TOO_LOW u400)
(define-constant BALANCE_TOO_LOW u401)
(define-constant UNAUTHORIZED u500)
(define-constant UNAUTHORIZED_CONTRACT u501)


;;              R E A D - O N L Y


;; @desc Returns a response containing a uint userId
;; @param user; The principal of the user

(define-read-only (get-user-id (user principal)) 
  (let ((userId (map-get? userIds user )))
    (begin 
      (asserts! (is-some userId) (err USER_DOES_NOT_EXIST))
      (ok (unwrap-panic userId))
    )
  )
)

;; @desc Retrieves the Fractal details of a certain NFT
;; @param nftId; The uint Fractal ID of the NFT

(define-read-only (get-nft-details (nftId uint))
  (let ((nft (map-get? fractalNfts nftId )))
    (begin 
      (asserts! (is-some nft) (err NFT_DOES_NOT_EXIST))
      (ok (unwrap-panic nft))
    )
  )
)

;; @desc Retrieves the last Fractal nftId fractalized

(define-read-only (get-last-token-id) (ok (- (var-get nftTip) u1)))

;; @desc Retrieves the last userId created

(define-read-only (get-last-user-id) (ok (- (var-get userIdTip) u1)))

;; @desc Retrieves the SIP9 URI of a certain Fractal NFT
;; @param nftId; The uint Fractal ID of the NFT

(define-read-only (get-token-uri (nftId uint))
  (let ((nft (map-get? fractalNfts nftId )))
    (begin 
      (asserts! (is-some nft) (err NFT_DOES_NOT_EXIST))
      (ok (get uri (unwrap-panic nft)))
    )
  )
)

;; @desc Retrieves a list of a user's owned Fractal NFTs
;; @param user; The principal of the user

(define-read-only (get-user-fractals (user principal))
  (ok (unwrap! 
    (map-get? userOwnedTokens (unwrap-panic (map-get? userIds user))) 
    (err USER_OWNED_TOKENS_DOES_NOT_EXIST)
  ))
)

;; @desc Retrieves the fractals balance of a principal for a certain NFT
;; @param user; The principal of the user
;; @param nftId; The uint Fractal ID of the NFT

(define-read-only (get-fractal-balance (user principal) (nftId uint))
  (let ((balance (map-get? userBalances { 
      userId: (unwrap-panic (get-user-id user)),
      nftId: nftId })))
    (if (is-none balance)
      (ok u0)
      (ok (unwrap-panic balance))
    )
  )
)

;; @desc Retrieves the total fractal balance of a principal over all NFTs
;; @param user; The principal of the user

(define-read-only (get-total-balance (user principal))
  (ok (ft-get-balance fractal user))
)

;; @desc Retrieves the total fractals in existence for a certain NFT
;; @param nftId; The uint Fractal ID of the NFT

(define-read-only (get-fractal-supply (nftId uint) )
  (ok (get totalSupply (unwrap-panic (get-nft-details nftId))))
)

;; @desc Retrieves the total fractals in existence for all NFTs

(define-read-only (get-total-supply)
  (ok (ft-get-supply fractal))
)

;; @desc Human readable name to display to the user

(define-read-only (get-name) (ok "Fractal") )

;; @desc Human readable symbol to display to the user

(define-read-only (get-symbol) (ok "FTL") )


;;              S T O R A G E


(define-map fractalNfts uint
  { totalSupply: uint, sip9Contract: principal, sip9Id: uint, 
    issuer: principal, uri: (optional (string-ascii 256)) }
)

(define-map userIds principal uint )

;; userId and nftId against uint balance
(define-map userBalances { userId: uint, nftId: uint } uint)

;; userId against list of uint nftIds
(define-map userOwnedTokens uint (list 512 uint))

(define-data-var userIdTip uint u1)
(define-data-var nftTip uint u1)

(define-data-var toRemove uint u0)

;; i didn't want to add this but otherwise people could use malicious contracts
;; to drain the nft vault through a malicious contract-call. whitelist is added
;; once through map-insert's at the end of the contract and cannot be changed
(define-map whitelist principal bool)


;;              P R I V A T E


(define-private (get-or-create-user-id (user principal)) 
  (let ((userId (map-get? userIds user)))
    (if (is-some userId) 
      (unwrap-panic userId)
      (let ((targetId (var-get userIdTip))) 
        (begin 
          (map-insert userIds user targetId )
          (var-set userIdTip (+ targetId u1))
          targetId
        )
      )
    )
  )
)

(define-private (contract-is-whitelisted (contract <sip9>))
  (is-some (map-get? whitelist (contract-of contract)))
)

(define-private (add-nft-to-owned-tokens (userId uint) (nftId uint))
  (let ((wrappedOwnedTokens (map-get? userOwnedTokens userId)))
    ;; if entry exists
    (if (is-some wrappedOwnedTokens) 
      (let ((ownedTokens (unwrap-panic wrappedOwnedTokens)))
        ;; if nftId is not already in ownedTokens
        (if (is-none (index-of ownedTokens nftId))
          (begin
            (map-set userOwnedTokens userId (unwrap-panic 
              (as-max-len? (append ownedTokens nftId) u512)
            ))
            ;; already owned, so no change
            (ok true)
          )
          (ok true)
        )
      )
      (begin 
        (map-insert userOwnedTokens userId (list nftId) )
        (ok true)
      )
    )
  )
)

(define-private (remove-nft-from-owned-tokens (userId uint) (nftId uint))
  ;; if we get here, we can assume that the user has the nftId in their list
  ;; so we don't need to check if there's an entry or not, or if the list is empty
  (begin 
    (var-set toRemove nftId)
    (map-set userOwnedTokens userId 
      (filter is-eq-toRemove (unwrap-panic (map-get? userOwnedTokens userId)))
    )
  )
)

(define-private (is-eq-toRemove (a uint))
  (is-eq a (var-get toRemove))
)

(define-private (get-or-create-fractal-balance (userId uint) (nftId uint))
  (let (
    (balance (map-get? userBalances { userId: userId, nftId: nftId }))
  )
    (if (is-some balance)
      (unwrap-panic balance)
      (begin 
        (map-insert userBalances { userId: userId, nftId: nftId } u0)
        u0
      )
    )
  )
)

(define-private (get-owner (nftContract <sip9>) (nftId uint))
  (unwrap-panic (contract-call? nftContract get-owner nftId))
)

(define-private (transfer-nft-to-vault (nftContract <sip9>) (nftId uint))
  (contract-call? nftContract transfer nftId tx-sender (as-contract tx-sender))
)

(define-private (transfer-nft-out-vault (nftContract <sip9>) (nftId uint))
  (let ((recipient tx-sender)) 
    (as-contract (contract-call? nftContract transfer nftId tx-sender recipient))
  )
)

(define-private (pull-token-uri (nftContract <sip9>) (nftId uint))
  (contract-call? nftContract get-token-uri nftId)
)


;;              W H I T E L I S T

(map-insert whitelist .test-nft true)