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
			break
		}
	}
	return intCode
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

	intCode[1] = 12
	intCode[2] = 2

	procesedIntCode := processIntCode(intCode)
	fmt.Println(procesedIntCode[0])
}
