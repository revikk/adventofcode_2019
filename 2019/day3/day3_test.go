package day3

import (
	"testing"
)

func TestPathToWire(t *testing.T) {
	input := "R1,U1,L1,D1"
	expect := wire{
		start: point{0, 0},
		end:   point{0, 0},
		path: wirePath{
			point{0, 0}: struct{}{},
			point{1, 0}: struct{}{},
			point{1, 1}: struct{}{},
			point{0, 1}: struct{}{},
			point{0, 0}: struct{}{},
		},
	}

	result := pathToWire(input)
	if result.start != expect.start || result.end != expect.end {
		t.Errorf("Fail, expect %v, got %v", expect, result)
	}

	if len(result.path) != len(expect.path) {
		t.Errorf("Fail, expect %v, got %v", expect, result)
	}
	for k, v := range result.path {
		if v != expect.path[k] {
			t.Errorf("Fail, expect %v, got %v", expect, result)
		}
	}
}
