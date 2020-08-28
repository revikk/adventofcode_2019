package main

import (
	"testing"
)

type inputExpect struct {
	input  []int
	expect []int
}

func TestProcessIntCode(t *testing.T) {
	data := []inputExpect{
		{[]int{1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50}, []int{3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50}},
		{[]int{1, 0, 0, 0, 99}, []int{2, 0, 0, 0, 99}},
		{[]int{2, 3, 0, 3, 99}, []int{2, 3, 0, 6, 99}},
		{[]int{2, 4, 4, 5, 99, 0}, []int{2, 4, 4, 5, 99, 9801}},
		{[]int{1, 1, 1, 4, 99, 5, 6, 0, 99}, []int{30, 1, 1, 4, 2, 5, 6, 0, 99}},
	}
	for _, testData := range data {
		result := processIntCode(testData.input)
		for i, v := range result {
			if v != testData.expect[i] {
				t.Fatalf("Fail, expect %v, got %v", testData.expect, result)
			}
		}
	}
}
