package main

import (
	"strconv"
	"strings"
)

type direction struct {
	axis     string
	distance int
}

type coordinate struct {
	x int
	y int
}

func manhattanDistance(wirePaths []string) int {
	coordinates1 := convertPathToCoordinates(wirePaths[0])
	coordinates2 := convertPathToCoordinates(wirePaths[1])
	var match []coordinate
	for _, coord1 := range coordinates1 {
		for _, coord2 := range coordinates2 {
			if coord1.x == coord2.x && coord1.y == coord2.y {
				match = append(match, coord1)
			}
		}
	}
}

func convertPathToCoordinates(path string) []coordinate {
	steps := strings.Split(path, ",")
	var dirs []direction
	for _, step := range steps {
		dir := convertStepToDirection(step)
		dirs = append(dirs, dir)
	}

	var coords []coordinate
	coords = append(coords, coordinate{0, 0})
	for _, dir := range dirs {
		switch dir.axis {
		case "x":
			lastCoor := coords[len(coords)-1]
			coords = append(coords, coordinate{lastCoor.x + dir.distance, lastCoor.y})
		case "y":
			lastCoor := coords[len(coords)-1]
			coords = append(coords, coordinate{lastCoor.x, lastCoor.y + dir.distance})
		}
	}
	return coords
}

func convertStepToDirection(step string) direction {
	splittedStep := strings.SplitN(step, "", 2)
	dis, _ := strconv.Atoi(splittedStep[1])
	dir := direction{}
	switch splittedStep[0] {
	case "R":
		dir = direction{axis: "x", distance: dis}
	case "L":
		dir = direction{axis: "x", distance: -1 * dis}
	case "U":
		dir = direction{axis: "y", distance: dis}
	case "D":
		dir = direction{axis: "y", distance: -1 * dis}
	}
	return dir
}

func main() {

}
