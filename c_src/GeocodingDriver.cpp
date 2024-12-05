#include <stdio.h> 
#include <string.h>
#include <stdlib.h>

#include <kdtree++/kdtree.hpp>
#include <vector>
#include <map>
#include <set>
#include <string>

// ========================================================================= //
// KD-Tree stuff.
// ========================================================================= //

using namespace std;

static const double DEG_TO_RAD = 0.017453292519943295769236907684886;
static const double EARTH_RADIUS_IN_METERS = 6372797.560856;
static const double SEMI_MAJOR_AXIS = 6378137;
// static const double RECIPROCAL_OF_FLATTENING = 298.257223563;
// static const double SEMI_MINOR_AXIS = 6356752.3142;
static const double FIRST_ECCENTRICITY_SQUARED = 6.69437999014e-3;
// static const double SECOND_ECCENTRICITY_SQUARED = 6.73949674228e-3;

struct TNode {
    typedef double value_type;
    double coordinates[3];
    double latitude;
    double longitude;

    // label
    string name;
    int id;
    string continent;
    char country[2];
    
    TNode(double inLatitude, double inLongitude) {
        latitude = inLatitude;
        longitude = inLongitude;
        double sin_latitude = sin(latitude * DEG_TO_RAD);
        double cos_latitude = cos(latitude * DEG_TO_RAD);
        double chi = sqrt(1 - FIRST_ECCENTRICITY_SQUARED * sin_latitude * sin_latitude);
        double normal = SEMI_MAJOR_AXIS / chi;
        coordinates[0] = normal * cos_latitude * cos(longitude * DEG_TO_RAD);
        coordinates[1] = normal * cos_latitude * sin(longitude * DEG_TO_RAD);
        coordinates[2] = normal * (1 - FIRST_ECCENTRICITY_SQUARED) * sin_latitude;
    }

    value_type operator[](size_t n) const {
        return coordinates[n];
    }

    double haversine_distance(const TNode &node) const {
        double fromLatitude = latitude;
        double toLatitude = node.latitude;
        double fromLongitude = longitude;
        double toLongitude = node.longitude;
    
        double latitudeArc  = (fromLatitude - toLatitude) * DEG_TO_RAD;
        double longitudeArc = (fromLongitude - toLongitude) * DEG_TO_RAD;
        double latitudeH = sin(latitudeArc / 2);
        latitudeH *= latitudeH;
        double lontitudeH = sin(longitudeArc / 2);
        lontitudeH *= lontitudeH;
        double tmp = cos(fromLatitude*DEG_TO_RAD) * cos(toLatitude*DEG_TO_RAD);
        return EARTH_RADIUS_IN_METERS * 2.0 * asin(sqrt(latitudeH + tmp*lontitudeH));
    }
};

typedef KDTree::KDTree<3, TNode> TKDTree;

int read_integer(char **line) {
  return strtol(*line, line, 10);
}

double read_double(char **line) {
  return strtod(*line, line);
}

void read_string(char **line, char *end, char *target, int target_size) {
  char *string_end = *line;
  while (*string_end != '\t' && *string_end != '\n' && string_end < end) {
    string_end++;
  }
  size_t string_size = string_end - *line;
  if (string_size > target_size - 1) {
    string_size = target_size - 1;
  }
  memcpy(target, *line, string_size);
  target[string_size] = '\0';
  *line = string_end + 1;
}

// Load database of geonameId, latitude, longitude, continent, country code, city name
bool load_db(TKDTree& tree, const char* path) {
    FILE* db_file = fopen(path, "r");
    char* line;
    char name[1024];
    char continent[1024];
    size_t line_size;
    int line_nb = 1;
    double latitude, longitude;
    int id;
    char country_code[2];

    while ((line = fgetln(db_file, &line_size))) {
        char* end = line + line_size;

        id = read_integer(&line);
        if (line >= end) {
            fprintf(stderr, "Could not parse geonameId at line %i\n", line_nb);
            break;
        }

        latitude = read_double(&line);
        if (line >= end) {
            fprintf(stderr, "Could not parse latitude at line %i\n", line_nb);
            break;
        }
        longitude = read_double(&line);
        if (line >= end) {
            fprintf(stderr, "Could not parse longitude at line %i\n", line_nb);
            break;
        }
        if (*line != '\t') {
            fprintf(stderr, "Could not parse continent at line %i\n", line_nb);
            break;
        }
        line++;

        read_string(&line, end, continent, sizeof(continent));
        country_code[0] = line[0];
        country_code[1] = line[1];
        line+=3;
        read_string(&line, end, name, sizeof(name));
        
        TNode node(latitude, longitude);
        node.id = id;
        node.country[0] = country_code[0];
        node.country[1] = country_code[1];
        node.name = string(name);
        node.continent = string(continent);
        tree.insert(node);
        line_nb++;
    }
    
    fclose(db_file);
    
    return true;
}

// ========================================================================= //
// Main entry point.
// ========================================================================= //

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "syntax error. %s <database_file>\n", argv[0]);
        return 1;
    }
    
    // Open the text database specified in argv.
    TKDTree tree;
    if (!load_db(tree, argv[1])) {
        return 1;
    }
    
    // Loop until the stdin is closed.
    char buffer_size[2];
    char buffer[1024];
    char* line;
    while (fread(buffer_size, sizeof(buffer_size), 1, stdin) == 1) {
        int nb_bytes = buffer_size[0] << 8 | buffer_size[1];
        if (nb_bytes > 1024) {
            fprintf(stderr, "Unexpected packet size %i !\n", nb_bytes);
            break;
        }
        if (fread(buffer, nb_bytes, 1, stdin) != 1) {
            fprintf(stderr, "Unexpected input !\n");
            break;
        }
        line = buffer;
        double latitude = strtod(line, &line);
        double longitude = strtod(line, &line);

        TNode searchNode(latitude, longitude);
        const TNode& nearestNode = *tree.find_nearest(searchNode).first;

        nb_bytes = snprintf(buffer, sizeof(buffer), "%f\t%d\t%s\t%c%c\t%s", nearestNode.haversine_distance(searchNode), nearestNode.id, nearestNode.continent.c_str(), nearestNode.country[0], nearestNode.country[1], nearestNode.name.c_str());
        buffer_size[0] = nb_bytes / 256;
        buffer_size[1] = nb_bytes & 0xFF;
        if (fwrite(buffer_size, sizeof(buffer_size), 1, stdout) != 1) {
            fprintf(stderr, "Could not write packet size !\n");
            break;
        }
        if (fwrite(buffer, nb_bytes, 1, stdout) != 1) {
            fprintf(stderr, "Could not write packet !\n");
            break;
        }
        fflush(stdout);
    }

    return 0;
}
