package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
)

func main() {
	file, err := os.Open("input")
	defer file.Close()
	if err != nil {
		log.Fatal(err)
	}

	scanner := bufio.NewScanner(file)
	var result float64 = 0
	for scanner.Scan() {
		mass, err := strconv.Atoi(scanner.Text())
		if err != nil {
			log.Fatal(err)
		}
		fuel := math.Trunc(float64(mass)/float64(3)) - 2
		result += fuel
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
	fmt.Println(result)
}
