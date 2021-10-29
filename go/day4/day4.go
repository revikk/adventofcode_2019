package day4

import (
	"sort"
	"strconv"
	"strings"
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
	if !sorted {
		return false
	}
	for i := 0; i < len(strPassword); i++ {
		lastIndex := strings.LastIndexByte(strPassword, strPassword[i])
		if (lastIndex - i) == 1 {
			firstIndex := strings.IndexByte(strPassword, strPassword[i])
			if firstIndex == i {
				return true
			}
		}
	}
	return false
}
