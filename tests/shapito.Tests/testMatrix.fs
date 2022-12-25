﻿module DoMyHomework.Tests.testMatrix

// Matrix from SuiteSparse Matrix Collection
// link: https://sparse.tamu.edu/Newman/football
// size: 115x115
let bigMArray = [|(2, 1, 1); (5, 1, 1); (10, 1, 1); (17, 1, 1); (24, 1, 1); (34, 1, 1)
                  (36, 1, 1); (42, 1, 1); (66, 1, 1); (91, 1, 1); (94, 1, 1); (105, 1, 1)
                  (26, 2, 1); (28, 2, 1); (34, 2, 1); (38, 2, 1); (46, 2, 1); (58, 2, 1)
                  (90, 2, 1); (102, 2, 1); (104, 2, 1); (106, 2, 1); (110, 2, 1); (4, 3, 1)
                  (7, 3, 1); (14, 3, 1); (15, 3, 1); (16, 3, 1); (48, 3, 1); (61, 3, 1)
                  (65, 3, 1); (73, 3, 1); (75, 3, 1); (101, 3, 1); (107, 3, 1); (6, 4, 1)
                  (12, 4, 1); (27, 4, 1); (41, 4, 1); (53, 4, 1); (59, 4, 1); (73, 4, 1)
                  (75, 4, 1); (82, 4, 1); (85, 4, 2); (103, 4, 1); (6, 5, 1); (10, 5, 1)
                  (17, 5, 1); (24, 5, 1); (29, 5, 1); (42, 5, 1); (70, 5, 1); (94, 5, 1)
                  (105, 5, 1); (109, 5, 1); (11, 6, 1); (12, 6, 1); (53, 6, 1); (75, 6, 1)
                  (82, 6, 1); (85, 6, 1); (91, 6, 1); (98, 6, 1); (99, 6, 1); (108, 6, 1)
                  (8, 7, 1); (33, 7, 1); (40, 7, 1); (48, 7, 1); (56, 7, 1); (59, 7, 1)
                  (61, 7, 1); (65, 7, 1); (86, 7, 1); (101, 7, 1); (107, 7, 1); (9, 8, 1)
                  (22, 8, 1); (23, 8, 1); (41, 8, 1); (69, 8, 1); (74, 8, 1); (78, 8, 1)
                  (79, 8, 1); (83, 8, 1); (109, 8, 1); (112, 8, 1); (10, 9, 1); (22, 9, 1)
                  (23, 9, 1); (42, 9, 1); (52, 9, 1); (69, 9, 1); (78, 9, 1); (79, 9, 1)
                  (91, 9, 1); (112, 9, 1); (17, 10, 1); (23, 10, 1); (24, 10, 1); (42, 10, 1)
                  (65, 10, 1); (94, 10, 1); (105, 10, 1); (109, 10, 1); (12, 11, 1); (61, 11, 1)
                  (73, 11, 1); (75, 11, 1); (82, 11, 1); (85, 11, 1); (99, 11, 1); (103, 11, 1)
                  (108, 11, 1); (25, 12, 1); (29, 12, 1); (51, 12, 1); (70, 12, 1); (91, 12, 1)
                  (98, 12, 1); (105, 12, 1); (14, 13, 1); (15, 13, 1); (18, 13, 1); (19, 13, 1)
                  (27, 13, 1); (35, 13, 1); (37, 13, 1); (39, 13, 1); (44, 13, 1); (86, 13, 1)
                  (16, 14, 1); (33, 14, 1); (40, 14, 1); (46, 14, 1); (61, 14, 1); (65, 14, 1)
                  (101, 14, 1); (107, 14, 1); (111, 14, 1); (16, 15, 1); (27, 15, 1); (39, 15, 1)
                  (44, 15, 1); (55, 15, 1); (72, 15, 1); (86, 15, 1); (100, 15, 2); (33, 16, 1)
                  (40, 16, 1); (48, 16, 1); (61, 16, 1); (69, 16, 1); (93, 16, 1); (101, 16, 1)
                  (107, 16, 1); (115, 16, 1); (18, 17, 1); (24, 17, 1); (39, 17, 1); (42, 17, 1)
                  (68, 17, 1); (82, 17, 1); (94, 17, 1); (105, 17, 1); (21, 18, 1); (28, 18, 2)
                  (59, 18, 1); (63, 18, 1); (66, 18, 1); (88, 18, 1); (96, 18, 1); (97, 18, 1)
                  (114, 18, 1); (20, 19, 1); (32, 19, 1); (35, 19, 1); (37, 19, 1); (39, 19, 1)
                  (43, 19, 1); (55, 19, 1); (62, 19, 1); (72, 19, 1); (100, 19, 1); (30, 20, 1)
                  (31, 20, 1); (34, 20, 1); (36, 20, 1); (37, 20, 1); (45, 20, 1); (56, 20, 1)
                  (80, 20, 1); (95, 20, 1); (102, 20, 1); (22, 21, 1); (37, 21, 1); (63, 21, 1)
                  (66, 21, 1); (71, 21, 1); (76, 21, 1); (77, 21, 1); (88, 21, 1); (97, 21, 1)
                  (114, 21, 1); (23, 22, 1); (33, 22, 1); (47, 22, 1); (52, 22, 1); (69, 22, 1)
                  (78, 22, 1); (109, 22, 1); (112, 22, 1); (24, 23, 1); (48, 23, 1); (52, 23, 1)
                  (69, 23, 1); (78, 23, 1); (79, 23, 1); (109, 23, 1); (42, 24, 1); (79, 24, 1)
                  (91, 24, 1); (94, 24, 1); (105, 24, 1); (112, 24, 1); (26, 25, 1); (29, 25, 1)
                  (51, 25, 1); (67, 25, 1); (70, 25, 1); (85, 25, 1); (88, 25, 1); (91, 25, 1)
                  (111, 25, 1); (34, 26, 1); (38, 26, 1); (46, 26, 1); (54, 26, 1); (90, 26, 1)
                  (104, 26, 1); (106, 26, 1); (107, 26, 1); (110, 26, 1); (28, 27, 1); (35, 27, 1)
                  (39, 27, 1); (43, 27, 1); (44, 27, 1); (62, 27, 1); (86, 27, 1); (57, 28, 1)
                  (63, 28, 1); (64, 28, 1); (66, 28, 1); (71, 28, 1); (77, 28, 1); (96, 28, 1)
                  (97, 28, 1); (39, 29, 1); (51, 29, 1); (70, 29, 1); (79, 29, 1); (91, 29, 1)
                  (114, 29, 1); (31, 30, 1); (36, 30, 1); (43, 30, 1); (56, 30, 1); (80, 30, 1)
                  (81, 30, 1); (83, 30, 1); (92, 30, 1); (95, 30, 1); (102, 30, 1); (36, 31, 1)
                  (45, 31, 1); (51, 31, 1); (56, 31, 1); (80, 31, 1); (83, 31, 1); (95, 31, 1)
                  (102, 31, 1); (110, 31, 1); (33, 32, 1); (35, 32, 1); (44, 32, 1); (55, 32, 1)
                  (56, 32, 1); (62, 32, 1); (72, 32, 1); (80, 32, 1); (86, 32, 1); (100, 32, 1)
                  (40, 33, 1); (48, 33, 1); (50, 33, 1); (65, 33, 1); (101, 33, 1); (107, 33, 1)
                  (38, 34, 1); (46, 34, 1); (90, 34, 1); (104, 34, 1); (106, 34, 1); (110, 34, 1)
                  (36, 35, 1); (43, 35, 1); (55, 35, 1); (62, 35, 1); (72, 35, 1); (95, 35, 1)
                  (100, 35, 1); (45, 36, 1); (56, 36, 1); (80, 36, 1); (93, 36, 1); (95, 36, 1)
                  (102, 36, 1); (38, 37, 1); (44, 37, 1); (59, 37, 1); (60, 37, 1); (46, 38, 1)
                  (81, 38, 1); (90, 38, 1); (96, 38, 1); (104, 38, 1); (106, 38, 1); (110, 38, 1)
                  (40, 39, 1); (44, 39, 1); (55, 39, 1); (72, 39, 1); (86, 39, 1); (48, 40, 1)
                  (55, 40, 1); (61, 40, 1); (83, 40, 1); (101, 40, 1); (107, 40, 1); (42, 41, 1)
                  (52, 41, 1); (53, 41, 1); (73, 41, 1); (75, 41, 1); (82, 41, 1); (99, 41, 1)
                  (103, 41, 1); (108, 41, 1); (68, 42, 1); (94, 42, 1); (105, 42, 1); (44, 43, 1)
                  (58, 43, 1); (64, 43, 1); (62, 44, 1); (71, 44, 1); (80, 44, 1); (86, 44, 1)
                  (46, 45, 1); (49, 45, 1); (58, 45, 1); (67, 45, 1); (76, 45, 1); (87, 45, 1)
                  (92, 45, 1); (113, 45, 1); (63, 46, 1); (90, 46, 1); (104, 46, 1); (106, 46, 1)
                  (110, 46, 1); (48, 47, 1); (50, 47, 1); (54, 47, 1); (68, 47, 1); (74, 47, 1)
                  (84, 47, 1); (89, 47, 1); (111, 47, 1); (112, 47, 1); (115, 47, 1); (61, 48, 1)
                  (62, 48, 1); (65, 48, 1); (101, 48, 1); (50, 49, 1); (54, 49, 1); (58, 49, 1)
                  (67, 49, 1); (76, 49, 1); (87, 49, 1); (92, 49, 1); (93, 49, 1); (97, 49, 1)
                  (99, 49, 1); (54, 50, 1); (68, 50, 1); (74, 50, 1); (84, 50, 1); (85, 50, 1)
                  (89, 50, 1); (111, 50, 1); (115, 50, 1); (52, 51, 1); (69, 51, 1); (70, 51, 1)
                  (79, 51, 1); (91, 51, 1); (69, 52, 1); (78, 52, 1); (79, 52, 1); (102, 52, 1)
                  (109, 52, 1); (112, 52, 1); (54, 53, 1); (73, 53, 1); (75, 53, 1); (85, 53, 1)
                  (99, 53, 1); (103, 53, 1); (113, 53, 1); (68, 54, 1); (74, 54, 1); (84, 54, 1)
                  (87, 54, 1); (89, 54, 1); (111, 54, 1); (115, 54, 1); (56, 55, 1); (62, 55, 1)
                  (72, 55, 1); (100, 55, 1); (80, 56, 1); (90, 56, 1); (95, 56, 1); (102, 56, 1)
                  (58, 57, 1); (63, 57, 1); (66, 57, 1); (71, 57, 1); (77, 57, 1); (88, 57, 1)
                  (96, 57, 1); (97, 57, 1); (107, 57, 1); (76, 58, 1); (87, 58, 1); (92, 58, 1)
                  (93, 58, 1); (113, 58, 1); (60, 59, 1); (64, 59, 1); (89, 59, 1); (98, 59, 1)
                  (102, 59, 1); (115, 59, 1); (61, 60, 1); (64, 60, 1); (67, 60, 1); (77, 60, 1)
                  (98, 60, 1); (114, 60, 1); (65, 61, 1); (72, 61, 1); (107, 61, 1); (63, 62, 1)
                  (72, 62, 1); (93, 62, 1); (100, 62, 1); (71, 63, 1); (77, 63, 1); (88, 63, 1)
                  (96, 63, 1); (106, 63, 1); (65, 64, 1); (66, 64, 1); (98, 64, 1); (110, 64, 1)
                  (113, 64, 1); (101, 65, 1); (107, 65, 1); (112, 65, 1); (67, 66, 1); (71, 66, 1)
                  (88, 66, 1); (97, 66, 1); (114, 66, 1); (76, 67, 1); (77, 67, 1); (87, 67, 1)
                  (92, 67, 1); (93, 67, 1); (113, 67, 1); (69, 68, 1); (74, 68, 1); (84, 68, 1)
                  (89, 68, 1); (105, 68, 1); (111, 68, 1); (115, 68, 1); (79, 69, 1); (109, 69, 1)
                  (112, 69, 1); (71, 70, 1); (84, 70, 1); (89, 70, 1); (91, 70, 1); (92, 70, 1)
                  (96, 70, 1); (77, 71, 1); (96, 71, 1); (104, 71, 1); (114, 71, 1); (73, 72, 1)
                  (100, 72, 1); (75, 73, 1); (82, 73, 1); (103, 73, 1); (105, 73, 1); (108, 73, 1)
                  (75, 74, 1); (78, 74, 1); (84, 74, 1); (89, 74, 1); (111, 74, 1); (115, 74, 1)
                  (83, 75, 1); (85, 75, 1); (103, 75, 1); (77, 76, 1); (87, 76, 1); (93, 76, 1)
                  (108, 76, 1); (113, 76, 1); (96, 77, 1); (97, 77, 1); (114, 77, 1); (79, 78, 1)
                  (83, 78, 1); (99, 78, 1); (109, 78, 1); (112, 78, 1); (109, 79, 1); (112, 79, 1)
                  (81, 80, 1); (95, 80, 1); (102, 80, 1); (110, 80, 1); (83, 81, 1); (86, 81, 1)
                  (87, 81, 1); (92, 81, 1); (94, 81, 1); (95, 81, 1); (106, 81, 1); (111, 81, 1)
                  (83, 82, 1); (84, 82, 1); (85, 82, 1); (99, 82, 1); (108, 82, 1); (94, 83, 1)
                  (95, 83, 1); (101, 83, 1); (85, 84, 1); (89, 84, 1); (111, 84, 1); (115, 84, 1)
                  (99, 85, 1); (108, 85, 1); (100, 86, 1); (88, 87, 1); (92, 87, 1); (93, 87, 1)
                  (98, 87, 1); (96, 88, 1); (97, 88, 1); (105, 88, 1); (114, 88, 1); (90, 89, 1)
                  (108, 89, 1); (111, 89, 1); (115, 89, 1); (100, 90, 1); (104, 90, 1); (106, 90, 1)
                  (110, 90, 1); (93, 92, 1); (94, 92, 1); (113, 92, 1); (107, 93, 1); (113, 93, 1)
                  (105, 94, 1); (102, 95, 1); (114, 96, 1); (113, 97, 1); (114, 97, 1); (99, 98, 1)
                  (113, 98, 1); (103, 99, 1); (108, 99, 1); (101, 100, 1); (103, 101, 1); (104, 103, 1)
                  (108, 103, 1); (106, 104, 1); (110, 104, 1); (115, 105, 1); (110, 106, 1); (112, 109, 1)
                  (115, 111, 1)|]

let bigM = Array.toList bigMArray
