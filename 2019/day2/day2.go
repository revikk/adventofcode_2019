package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

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
	fmt.Println(intCode)
}
