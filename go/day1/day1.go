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
	return math.Trunc(mass/float64(3)) - 2
}

func totalFuelForMass(mass float64) float64 {
	var total float64
	fuel := massToFuel(mass)
	for fuel > 0 {
		total += fuel
		fuel = massToFuel(fuel)
	}
	return total
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
		// fuel := massToFuel(mass)
		fuel := totalFuelForMass(mass)
		result += fuel
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	fmt.Println(result)
}
