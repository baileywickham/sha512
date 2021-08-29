package main

import (
	"fmt"
	"math"
)

const w = 64

var K = [80]uint64{0x428a2f98d728ae22, 0x7137449123ef65cd, 0xb5c0fbcfec4d3b2f, 0xe9b5dba58189dbbc, 0x3956c25bf348b538, 0x59f111f1b605d019, 0x923f82a4af194f9b, 0xab1c5ed5da6d8118, 0xd807aa98a3030242, 0x12835b0145706fbe, 0x243185be4ee4b28c, 0x550c7dc3d5ffb4e2, 0x72be5d74f27b896f, 0x80deb1fe3b1696b1, 0x9bdc06a725c71235, 0xc19bf174cf692694, 0xe49b69c19ef14ad2, 0xefbe4786384f25e3, 0x0fc19dc68b8cd5b5, 0x240ca1cc77ac9c65, 0x2de92c6f592b0275, 0x4a7484aa6ea6e483, 0x5cb0a9dcbd41fbd4, 0x76f988da831153b5, 0x983e5152ee66dfab, 0xa831c66d2db43210, 0xb00327c898fb213f, 0xbf597fc7beef0ee4, 0xc6e00bf33da88fc2, 0xd5a79147930aa725, 0x06ca6351e003826f, 0x142929670a0e6e70, 0x27b70a8546d22ffc, 0x2e1b21385c26c926, 0x4d2c6dfc5ac42aed, 0x53380d139d95b3df, 0x650a73548baf63de, 0x766a0abb3c77b2a8, 0x81c2c92e47edaee6, 0x92722c851482353b, 0xa2bfe8a14cf10364, 0xa81a664bbc423001, 0xc24b8b70d0f89791, 0xc76c51a30654be30, 0xd192e819d6ef5218, 0xd69906245565a910, 0xf40e35855771202a, 0x106aa07032bbd1b8, 0x19a4c116b8d2d0c8, 0x1e376c085141ab53, 0x2748774cdf8eeb99, 0x34b0bcb5e19b48a8, 0x391c0cb3c5c95a63, 0x4ed8aa4ae3418acb, 0x5b9cca4f7763e373, 0x682e6ff3d6b2b8a3, 0x748f82ee5defb2fc, 0x78a5636f43172f60, 0x84c87814a1f0ab72, 0x8cc702081a6439ec, 0x90befffa23631e28, 0xa4506cebde82bde9, 0xbef9a3f7b2c67915, 0xc67178f2e372532b, 0xca273eceea26619c, 0xd186b8c721c0c207, 0xeada7dd6cde0eb1e, 0xf57d4f7fee6ed178, 0x06f067aa72176fba, 0x0a637dc5a2c898a6, 0x113f9804bef90dae, 0x1b710b35131c471b, 0x28db77f523047d84, 0x32caab7b40c72493, 0x3c9ebe0a15c9bebc, 0x431d67c49c100d4c, 0x4cc5d4becb3e42b6, 0x597f299cfc657e2a, 0x5fcb6fab3ad6faec, 0x6c44198c4a475817}
var H = [8]uint64{0x6a09e667f3bcc908, 0xbb67ae8584caa73b, 0x3c6ef372fe94f82b, 0xa54ff53a5f1d36f1, 0x510e527fade682d1, 0x9b05688c2b3e6c1f, 0x1f83d9abfb41bd6b, 0x5be0cd19137e2179}
var H_I = 1

func main() {
	message := pad(messageToBytes("abc"))
	Message := byteSliceToMessage(message)
	hash(Message)
	printHash()
}

func printHash() {
	output := ""
	for i := 0; i <= 7; i++ {
		output += fmt.Sprintf("%x", H[i])
	}
	println(output)
}

// Rotate left, x is w-bit word, n uint64 where 0 <= n < w
func rotl(n, x uint64) uint64 {
	return (x << n) | (x >> (w - n))
}

// Rotate Right, x is w-bit word, n uint64 where 0 <= n < w
func rotr(n, x uint64) uint64 {
	return (x >> n) | (x << (x - n))
}

func shr(n, x uint64) uint64 {
	return x >> n

}
func ch(x, y, z uint64) uint64 {
	return (x & y) ^ ((^x) & z)
}

func Maj(x, y, z uint64) uint64 {
	return (x & y) ^ (x & z) ^ (y & z)
}

func SIG_0(x uint64) uint64 {
	return rotr(28, x) ^ rotr(34, x) ^ rotr(39, x)
}

func SIG_1(x uint64) uint64 {
	return rotr(14, x) ^ rotr(18, x) ^ rotr(41, x)
}

func sig_0(x uint64) uint64 {
	return rotr(1, x) ^ rotr(8, x) ^ shr(7, x)
}

func sig_1(x uint64) uint64 {
	return rotr(19, x) ^ rotr(61, x) ^ shr(6, x)
}

func add(nums ...uint64) uint64 {
	s := uint64(0)
	for _, i := range nums {
		s += i % uint64((math.Pow(2, w)))

	}
	return s
}

func i64tobarr(val uint64) []byte {
	r := make([]byte, 8)
	for i := uint64(0); i < 8; i++ {
		r[i] = byte((val >> (i * 8)) & 0xff)
	}
	return r
}

func btoi64(val []byte) uint64 {
	r := uint64(0)
	for i := uint64(0); i < 8; i++ {
		r |= uint64(val[i]) << (8 * i)
	}
	return r
}

// returns a padded message which should be a multiple of 1024
func pad(message []byte) []byte {
	messageLenBytes := uint64(len(message) * 8)
	k := (896 - (messageLenBytes + 1)) % 1024

	zero_pad := ((k - 1) / 8)
	// append 1 lengthened to a byte
	message = append(message, (byte(0)+1)<<7)
	// append empty bytes
	message = append(message, make([]byte, zero_pad)...)
	// Append top 64 bits to array, truncate 128 bit length
	message = append(message, make([]byte, 8)...)
	// Append length
	message = append(message, i64tobarr(messageLenBytes)...)

	return message
}

func byteSliceToMessage(message []byte) [][]uint64 {

	Message := [][]uint64{}
	for i := 0; i < len(message)/128; i++ {
		messageBlock := []uint64{}
		workingMessage := message[i*128 : (i+1)*128]
		for w := 0; w < 128; w += 8 {
			messageBlock = append(messageBlock, btoi64(workingMessage[w:w+8]))
		}
		Message = append(Message, messageBlock)
	}
	return Message
}

func hash(Message [][]uint64) {
	for _, value := range Message {
		hashMessage(value)
	}
}

func hashMessage(messageBlock []uint64) {
	W := make([]uint64, 80)
	for t := 0; t <= 15; t++ {
		W[t] = messageBlock[t]
	}

	for t := 16; t <= 79; t++ {
		W[t] = add(sig_1(W[t-2]), W[t-7], sig_0(W[t-15]), W[t-16])
	}

	a := H[0]
	b := H[1]
	c := H[2]
	d := H[3]
	e := H[4]
	f := H[5]
	g := H[6]
	h := H[7]
	for t := 0; t <= 79; t++ {
		T_1 := add(h, SIG_1(e), ch(e, f, g), K[t], W[t])
		T_2 := add(SIG_0(a), Maj(a, b, c))
		h = g
		g = f
		f = e
		e = add(d, T_1)
		d = c
		c = b
		b = a
		a = add(T_1 + T_2)
	}
	H[0] = add(a, H[0])
	H[1] = add(b, H[1])
	H[2] = add(c, H[2])
	H[3] = add(d, H[3])
	H[4] = add(e, H[4])
	H[5] = add(f, H[5])
	H[6] = add(g, H[6])
	H[7] = add(h, H[7])
}

func messageToBytes(message string) []byte {
	return []byte(message)
}
