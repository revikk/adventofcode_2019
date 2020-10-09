package day4

import (
	"sort"
	"strconv"
)

func isValidPassword(password int) bool {
	strPassword := strconv.Itoa(password)
	if len(strPassword) != 6 {
		return false
	}
	sorted := sort.SliceIsSorted(strPassword,
		func(i, j int) bool {
			return strPassword[i] < strPassword[j]
		})
	if sorted {

	}
}
