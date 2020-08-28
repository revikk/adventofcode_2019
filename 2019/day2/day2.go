package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func processIntCode(intCode []int) []int {
	for i := 0; i <= len(intCode); i += 4 {
		switch intCode[i] {
		case 1:
			intCode[intCode[i+3]] = intCode[intCode[i+1]] + intCode[intCode[i+2]]
		case 2:
			intCode[intCode[i+3]] = intCode[intCode[i+1]] * intCode[intCode[i+2]]
		case 99:
			return intCode
		}
	}
	return intCode
}

func findNounAndVerbForOutput(intCode []int) (int, int) {
	var noun, verb int
	initIntCode := make([]int, len(intCode))
	copy(initIntCode, intCode)
	for noun = 0; noun <= 99; noun++ {
		for verb = 0; verb <= 99; verb++ {
			intCode[1] = noun
			intCode[2] = verb
			result := processIntCode(intCode)
			if result[0] == 19690720 {
				return noun, verb
			}
			copy(intCode, initIntCode)
		}
	}
	return noun, verb
}

func main() {
	file, err := os.Open("input")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var intCode []int
	for scanner.Scan() {
		stringCodes := strings.Split(scanner.Text(), ",")
		for _, stringCode := range stringCodes {
			code, err := strconv.Atoi(stringCode)
			if err != nil {
				log.Fatal(err)
			}
			intCode = append(intCode, code)
		}
	}
	if err = scanner.Err(); err != nil {
		log.Fatal(err)
	}

	// intCode[1] = 12
	// intCode[2] = 2
	// result := processIntCode(intCode)
	// fmt.Printf("Day2 part 1 answer %v\n", result[0])

	noun, verb := findNounAndVerbForOutput(intCode)
	fmt.Printf("Day2 part 2 answer %v\n", 100*noun+verb)
}
