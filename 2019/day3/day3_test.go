package main

import (
	"testing"
)

type inputExpect struct {
	input  []string
	expect int
}

func TestManhattanDistance(t *testing.T) {
	data := []inputExpect{
		{[]string{"R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"}, 159},
		{[]string{"R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"}, 135},
	}

	for _, testData := range data {
		result := manhattanDistance(testData.input)
		if result != testData.expect {
			t.Fatalf("Fail, expect %v, got %v", testData.expect, result)
		}
	}

}

func TestConvertStepToDirection(t *testing.T) {
	result := convertStepToDirection("R75")
	if result.axis != "x" || result.distance != 75 {
		t.Fatalf("Fail, expect %v, got %v", direction{"x", 75}, result)
	}

	result = convertStepToDirection("L75")
	if result.axis != "x" || result.distance != -75 {
		t.Fatalf("Fail, expect %v, got %v", direction{"x", -75}, result)
	}

	result = convertStepToDirection("U75")
	if result.axis != "y" || result.distance != 75 {
		t.Fatalf("Fail, expect %v, got %v", direction{"x", -75}, result)
	}

	result = convertStepToDirection("D75")
	if result.axis != "y" || result.distance != -75 {
		t.Fatalf("Fail, expect %v, got %v", direction{"x", -75}, result)
	}
}

func TestConverPathToCoordinates(t *testing.T) {
	result := convertPathToCoordinates("R75,D30,R83,U83,L12,D49,R71,U7,L72")
	expect := []coordinate{
		coordinate{0, 0}, coordinate{75, 0}, coordinate{75, -30},
		coordinate{158, -30}, coordinate{158, 53}, coordinate{146, 53}, coordinate{146, 4},
		coordinate{217, 4}, coordinate{217, 11}, coordinate{145, 11}}
	for i, v := range result {
		coord := expect[i]
		if v.x != coord.x || v.y != coord.y {
			t.Fatalf("Fail, expect %v, got %v", expect, result)
		}
	}
}
