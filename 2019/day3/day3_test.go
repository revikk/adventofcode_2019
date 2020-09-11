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

func TestCrossing(t *testing.T) {
	w1 := pathToWire("R1,U1,L1,D1")
	w2 := pathToWire("D1,R2,U2,L2")

	expect := []point{
		point{0, 1},
		point{1, 1},
	}

	result := crossing(w1, w2)
	for i, v := range result {
		if v != expect[i] {
			t.Errorf("Fail, expect %v, got %v", expect, result)
		}
	}
}
