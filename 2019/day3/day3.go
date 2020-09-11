package day3

import (
	"image"
	"log"
	"strconv"
	"strings"
)

type point image.Point
type wirePath map[point]struct{}
type wire struct {
	start point
	end   point
	path  wirePath
}

func (w *wire) addPoint(p point) {
	if _, ok := w.path[p]; !ok {
		w.path[p] = struct{}{}
	}
	w.end = p
}

func pathToWire(path string) wire {
	w := wire{
		start: point{0, 0},
		end:   point{0, 0},
		path:  wirePath{point{0, 0}: struct{}{}},
	}

	directions := strings.Split(path, ",")
	for _, dir := range directions {
		switch dir[:1] {
		case "R":
			steps := getSteps(dir)
			for i := 1; i <= steps; i++ {
				np := point{w.end.X + i, w.end.Y}
				w.addPoint(np)
			}
		case "L":
			steps := getSteps(dir)
			for i := 1; i <= steps; i++ {
				np := point{w.end.X - i, w.end.Y}
				w.addPoint(np)
			}
		case "U":
			steps := getSteps(dir)
			for i := 1; i <= steps; i++ {
				np := point{w.end.X, w.end.Y + i}
				w.addPoint(np)
			}
		case "D":
			steps := getSteps(dir)
			for i := 1; i <= steps; i++ {
				np := point{w.end.X, w.end.Y - i}
				w.addPoint(np)
			}
		}
	}
	return w
}

func getSteps(dir string) int {
	steps, err := strconv.Atoi(dir[1:])
	if err != nil {
		log.Fatal(err)
	}
	return steps
}

func crossing(w1, w2 wire) []point {
	cross := make([]point, 0)
	for k := range w1.path {
		if v, ok := w2.path[k]; ok {
			cross = append(cross, v)
		}
	}
	return cross
}
