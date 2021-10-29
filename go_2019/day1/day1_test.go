package main

import (
	"testing"
)

type inputExpected struct {
	input    float64
	expected float64
}

func TestMassToFuel(t *testing.T) {
	dataItems := []inputExpected{
		{12, 2},
		{14, 2},
		{1969, 654},
		{100756, 33583},
	}

	for _, item := range dataItems {
		result := massToFuel(item.input)
		checkResult(t, result, item.expected)
	}
}

func TestTotalFuelForMass(t *testing.T) {
	dataItems := []inputExpected{
		{14, 2},
		{1969, 966},
		{100756, 50346},
	}

	for _, item := range dataItems {
		result := totalFuelForMass(item.input)
		checkResult(t, result, item.expected)
	}
}

func checkResult(t *testing.T, result float64, expect float64) {
	if result != expect {
		t.Errorf("FAIL, expected %v, got %v", expect, result)
	}
}
