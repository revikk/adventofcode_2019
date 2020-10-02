package day4

import (
	"testing"
)

func TestIsValidPassword(t *testing.T) {
	input := []struct {
		in  int
		out bool
	}{
		{111111, true},
		{223450, false},
		{123789, false},
	}
	for _, test := range input {
		result := isValidPassword(test.in)
		if result != test.out {
			t.Errorf("Error, expect %v, got %v", test.out, result)
		}
	}
}
