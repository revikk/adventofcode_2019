package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
)

func massToFuel(mass float64) float64 {
	return math.Trunc(float64(mass)/float64(3)) - 2
}

func main() {
	file, err := os.Open("input")
	defer file.Close()
	if err != nil {
		log.Fatal(err)
	}

	scanner := bufio.NewScanner(file)
	var result float64 = 0
	for scanner.Scan() {
		mass, err := strconv.ParseFloat(scanner.Text(), 64)
		if err != nil {
			log.Fatal(err)
		}
		fuel := massToFuel(mass)
		result += fuel
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	fmt.Println(result)
}
