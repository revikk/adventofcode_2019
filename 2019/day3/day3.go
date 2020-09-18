package day3

import (
	"image"
	"log"
	"math"
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
}

func (w *wire) setEndPoint(p point) {
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
		np := point{0, 0}
		switch dir[:1] {
		case "R":
			steps := getSteps(dir)
			for i := 1; i <= steps; i++ {
				np = point{w.end.X + 1, w.end.Y}
				w.addPoint(np)
				w.setEndPoint(np)
			}
		case "L":
			steps := getSteps(dir)
			for i := 1; i <= steps; i++ {
				np = point{w.end.X - 1, w.end.Y}
				w.addPoint(np)
				w.setEndPoint(np)
			}
		case "U":
			steps := getSteps(dir)
			for i := 1; i <= steps; i++ {
				np = point{w.end.X, w.end.Y + 1}
				w.addPoint(np)
				w.setEndPoint(np)
			}
		case "D":
			steps := getSteps(dir)
			for i := 1; i <= steps; i++ {
				np = point{w.end.X, w.end.Y - 1}
				w.addPoint(np)
				w.setEndPoint(np)
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
		if k.X == 0 && k.Y == 0 {
			continue
		}
		if _, ok := w2.path[k]; ok {
			cross = append(cross, k)
		}
	}
	return cross
}

func manhattanDistance(p1, p2 string) uint {
	w1 := pathToWire(p1)
	w2 := pathToWire(p2)

	crossPoints := crossing(w1, w2)

	var distance float64 = 0.0
	for _, point := range crossPoints {
		// actually, the formula is |x1 - x2| + |y1 - y2|, but
		// control point is always in {0, 0} point. So the simplified formula is
		newDistance := math.Abs(float64(point.X)) + math.Abs(float64(point.Y))
		if distance == 0.0 {
			distance = math.Max(distance, newDistance)
		}
		distance = math.Min(distance, newDistance)
	}
	return uint(distance)
}
