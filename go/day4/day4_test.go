package day4

import (
	"testing"
)

func TestIsValidPassword(t *testing.T) {
	input := []struct {
		in  int
		out bool
	}{
		// {111111, true},
		// {223450, false},
		// {123789, false},
		{112233, true},
		{123444, false},
		{111122, true},
	}
	for _, test := range input {
		result := isValidPassword(test.in)
		if result != test.out {
			t.Errorf("Error, in %v, expect %v, got %v", test.in, test.out, result)
		}
	}
}

func TestDay4(t *testing.T) {
	start := 183564
	stop := 657474
	count := 0
	for i := start; i <= stop; i++ {
		if isValidPassword(i) == true {
			count++
		}
	}
	t.Logf("Day4 part 2 answer is %v", count)
}
