open Sig_mod
open Rule_sig_mod
open Rule_mod

let () =
  mod_rule_list :=
    [ ( "Creat faces",
        { hooks = [];
          left_nodes = [||];
          left_edges = [];
          node_match = [||];
          right_nodes =
            [| [2; 3]; [2; 3]; [2; 3]; [2; 3]; [2; 3]; [2; 3]; [2; 3]; [2; 3] |];
          right_edges =
            [ (0, 1, 0);
              (1, 2, 1);
              (2, 3, 0);
              (3, 4, 1);
              (4, 5, 0);
              (5, 6, 1);
              (6, 7, 0);
              (7, 0, 1) ];
          apply_cond = [];
          ebd_expr = []
        } );
      ( "connex removal",
        { hooks = [0];
          left_nodes = [| [0; 1; 2; 3] |];
          left_edges = [];
          node_match = [| -1 |];
          right_nodes = [||];
          right_edges = [];
          apply_cond = [];
          ebd_expr = []
        } );
      ( "alpha0 sewing",
        { hooks = [0; 1];
          left_nodes = [| [2; 3]; [2; 3] |];
          left_edges = [(0, 0, 0); (1, 1, 0)];
          node_match = [| 0; 1 |];
          right_nodes = [| [2; 3]; [2; 3] |];
          right_edges = [(0, 1, 0)];
          apply_cond = [];
          ebd_expr = [(1, "color", ColorEbd (Node 0))]
        } );
      ( "alpha1 sewing",
        { hooks = [0; 1];
          left_nodes = [| [3]; [3] |];
          left_edges = [(0, 0, 1); (1, 1, 1)];
          node_match = [| 0; 1 |];
          right_nodes = [| [3]; [3] |];
          right_edges = [(0, 1, 1)];
          apply_cond = [];
          ebd_expr =
            [(1, "point", PointEbd (Node 0)); (1, "color", ColorEbd (Node 0))]
        } );
      ( "alpha2 sewing",
        { hooks = [0; 1];
          left_nodes = [| [0]; [0] |];
          left_edges = [(0, 0, 2); (1, 1, 2)];
          node_match = [| 0; 1 |];
          right_nodes = [| [0]; [0] |];
          right_edges = [(0, 1, 2)];
          apply_cond = [];
          ebd_expr = [(1, "point", PointEbd (Node 0))]
        } );
      ( "alpha3 sewing",
        { hooks = [0; 1];
          left_nodes = [| [0; 1]; [0; 1] |];
          left_edges = [(0, 0, 3); (1, 1, 3)];
          node_match = [| 0; 1 |];
          right_nodes = [| [0; 1]; [0; 1] |];
          right_edges = [(0, 1, 3)];
          apply_cond = [];
          ebd_expr =
            [(1, "point", PointEbd (Node 0)); (1, "color", ColorEbd (Node 0))]
        } );
      ( "alpha0 unsewing",
        { hooks = [0];
          left_nodes = [| [2; 3]; [2; 3] |];
          left_edges = [(0, 1, 0)];
          node_match = [| 0; 1 |];
          right_nodes = [| [2; 3]; [2; 3] |];
          right_edges = [(0, 0, 0); (1, 1, 0)];
          apply_cond = [];
          ebd_expr = []
        } );
      ( "alpha1 unsewing",
        { hooks = [0];
          left_nodes = [| [3]; [3] |];
          left_edges = [(0, 1, 1)];
          node_match = [| 0; 1 |];
          right_nodes = [| [3]; [3] |];
          right_edges = [(0, 0, 1); (1, 1, 1)];
          apply_cond = [];
          ebd_expr = []
        } );
      ( "alpha2 unsewing",
        { hooks = [0];
          left_nodes = [| [0]; [0] |];
          left_edges = [(0, 1, 2)];
          node_match = [| 0; 1 |];
          right_nodes = [| [0]; [0] |];
          right_edges = [(0, 0, 2); (1, 1, 2)];
          apply_cond = [];
          ebd_expr = []
        } );
      ( "alpha3 unsewing",
        { hooks = [0];
          left_nodes = [| [0; 1]; [0; 1] |];
          left_edges = [(0, 1, 3)];
          node_match = [| 0; 1 |];
          right_nodes = [| [0; 1]; [0; 1] |];
          right_edges = [(0, 0, 3); (1, 1, 3)];
          apply_cond = [];
          ebd_expr = []
        } );
      ( "vertex extrusion",
        { hooks = [0];
          left_nodes = [| [1; 2; 3] |];
          left_edges = [(0, 0, 0)];
          node_match = [| 0 |];
          right_nodes = [| [1; 2; 3]; [1; 2; 3] |];
          right_edges = [(0, 1, 0)];
          apply_cond = [];
          ebd_expr = [(1, "point", Plus (PointEbd (Node 0), ConstVar 'v'))]
        } );
      ( "edge extrusion",
        { hooks = [0];
          left_nodes = [| [0; 2; 3] |];
          left_edges = [(0, 0, 1)];
          node_match = [| 0 |];
          right_nodes = [| [0; 2; 3]; [-1; 2; 3]; [-1; 2; 3]; [0; 2; 3] |];
          right_edges = [(0, 1, 1); (1, 2, 0); (2, 3, 1)];
          apply_cond = [];
          ebd_expr = [(2, "point", Plus (PointEbd (Node 0), ConstVar 'v'))]
        } );
      ( "face extrusion",
        { hooks = [0];
          left_nodes = [| [0; 1; 3] |];
          left_edges = [(0, 0, 2)];
          node_match = [| 0 |];
          right_nodes =
            [| [0; 1; 3];
               [0; -1; 3];
               [-1; 2; 3];
               [-1; 2; 3];
               [0; -1; 3];
               [0; 1; 3]
            |];
          right_edges = [(0, 1, 2); (1, 2, 1); (2, 3, 0); (3, 4, 1); (4, 5, 2)];
          apply_cond = [];
          ebd_expr =
            [ (3, "point", Plus (PointEbd (Node 0), ConstVar 'v'));
              (1, "color", ColorEbd (Node 0));
              (5, "color", ColorEbd (Node 0)) ]
        } );
      ( "edge cone",
        { hooks = [0];
          left_nodes = [| [0; 2; 3] |];
          left_edges = [(0, 0, 1)];
          node_match = [| 0 |];
          right_nodes = [| [0; 2; 3]; [-1; 2; 3]; [1; 2; 3] |];
          right_edges = [(0, 1, 1); (1, 2, 0)];
          apply_cond = [];
          ebd_expr =
            [ ( 2,
                "point",
                Plus (Mean (OrbitPoint ([0; 2; 3], Node 0)), ConstVar 'v') ) ]
        } );
      ( "face cone",
        { hooks = [0];
          left_nodes = [| [0; 1; 3] |];
          left_edges = [(0, 0, 2)];
          node_match = [| 0 |];
          right_nodes = [| [0; 1; 3]; [0; -1; 3]; [-1; 2; 3]; [1; 2; 3] |];
          right_edges = [(0, 1, 2); (1, 2, 1); (2, 3, 0)];
          apply_cond = [];
          ebd_expr =
            [ ( 3,
                "point",
                Plus (Mean (OrbitPoint ([0; 1; 3], Node 0)), ConstVar 'v') );
              (1, "color", ColorEbd (Node 0)) ]
        } );
      ( "edge splitting",
        { hooks = [0];
          left_nodes = [| [2; 3]; [2; 3] |];
          left_edges = [(0, 1, 0)];
          node_match = [| 0; 3 |];
          right_nodes = [| [2; 3]; [2; 3]; [2; 3]; [2; 3] |];
          right_edges = [(0, 1, 0); (1, 2, 1); (2, 3, 0)];
          apply_cond = [];
          ebd_expr = [(1, "point", Mean (OrbitPoint ([0; 2; 3], Node 0)))]
        } );
      ( "face triangulation",
        { hooks = [0];
          left_nodes = [| [0; 1; 3] |];
          left_edges = [];
          node_match = [| 0 |];
          right_nodes = [| [0; -1; 3]; [-1; 2; 3]; [1; 2; 3] |];
          right_edges = [(0, 1, 1); (1, 2, 0)];
          apply_cond = [];
          ebd_expr =
            [ (2, "point", Mean (OrbitPoint ([0; 1; 3], Node 0)));
              ( 0,
                "color",
                Mean (EnsExpr [ColorEbd (Node 0); ColorEbd (Alpha2 (Node 0))])
              ) ]
        } );
      ( "volume triangulation",
        { hooks = [0];
          left_nodes = [| [0; 1; 2] |];
          left_edges = [];
          node_match = [| 0 |];
          right_nodes = [| [0; 1; -1]; [0; -1; 3]; [-1; 2; 3]; [1; 2; 3] |];
          right_edges = [(0, 1, 2); (1, 2, 1); (2, 3, 0)];
          apply_cond = [];
          ebd_expr =
            [ (3, "point", Mean (OrbitPoint ([0; 1; 2], Node 0)));
              ( 1,
                "color",
                Mean (EnsExpr [ColorEbd (Node 0); ColorEbd (Alpha2 (Node 0))])
              ) ]
        } );
      ( "vertex rounding",
        { hooks = [0];
          left_nodes = [| [1; 2; 3] |];
          left_edges = [];
          node_match = [| 0 |];
          right_nodes = [| [-1; 2; 3]; [0; -1; 3]; [0; 1; -1]; [0; 1; 2] |];
          right_edges = [(0, 1, 1); (1, 2, 2); (2, 3, 3)];
          apply_cond = [];
          ebd_expr =
            [ ( 0,
                "point",
                Plus
                  ( Scal (0.7, PointEbd (Node 0)),
                    Scal (0.3, PointEbd (Alpha0 (Node 0))) ) );
              (2, "color", Mean (OrbitColor ([1; 2], Node 0))) ]
        } );
      ( "dug vertex rounding",
        { hooks = [0];
          left_nodes = [| [1; 2; 3] |];
          left_edges = [];
          node_match = [| 0 |];
          right_nodes = [| [-1; 2; 3]; [0; -1; 3]; [0; 1; -1] |];
          right_edges = [(0, 1, 1); (1, 2, 2); (2, 2, 3)];
          apply_cond = [];
          ebd_expr =
            [ ( 0,
                "point",
                Plus
                  ( Scal (0.7, PointEbd (Node 0)),
                    Scal (0.3, PointEbd (Alpha0 (Node 0))) ) );
              (2, "color", Mean (OrbitColor ([1; 2], Node 0))) ]
        } );
      ( "edge rounding",
        { hooks = [0];
          left_nodes = [| [0; 1; 2; 3] |];
          left_edges = [];
          node_match = [| 0 |];
          right_nodes =
            [| [0; 1; -1; 3];
               [0; -1; -1; -1];
               [-1; -1; 0; -1];
               [-1; 1; 0; -1];
               [0; -1; -1; 2];
               [-1; -1; 0; -1];
               [-1; 1; 0; -1]
            |];
          right_edges =
            [ (0, 1, 2);
              (1, 2, 1);
              (2, 3, 2);
              (1, 4, 3);
              (2, 5, 3);
              (3, 6, 3);
              (4, 5, 1);
              (5, 6, 2) ];
          apply_cond = [];
          ebd_expr =
            [ ( 0,
                "point",
                Plus
                  ( Scal (0.7, PointEbd (Node 0)),
                    Scal (0.3, Mean (OrbitPoint ([0; 1; 3], Node 0))) ) );
              (1, "color", Mean (OrbitColor ([0; 2], Node 0)));
              (3, "color", Mean (OrbitColor ([1; 2], Node 0))) ]
        } );
      ( "dug edge rounding",
        { hooks = [0];
          left_nodes = [| [0; 1; 2; 3] |];
          left_edges = [];
          node_match = [| 0 |];
          right_nodes =
            [| [0; 1; -1; 3];
               [0; -1; -1; -1];
               [-1; -1; 0; -1];
               [-1; 1; 0; -1]
            |];
          right_edges =
            [(0, 1, 2); (1, 2, 1); (2, 3, 2); (1, 1, 3); (2, 2, 3); (3, 3, 3)];
          apply_cond = [];
          ebd_expr =
            [ ( 0,
                "point",
                Plus
                  ( Scal (0.7, PointEbd (Node 0)),
                    Scal (0.3, Mean (OrbitPoint ([0; 1; 3], Node 0))) ) );
              (1, "color", Mean (OrbitColor ([0; 2], Node 0)));
              (3, "color", Mean (OrbitColor ([1; 2], Node 0))) ]
        } );
      ( "neg edge rounding",
        { hooks = [0];
          left_nodes = [| [0; 1; 2; 3] |];
          left_edges = [];
          node_match = [| -1 |];
          right_nodes = [| [0; -1; -1; 2]; [-1; -1; 0; -1]; [-1; 1; 0; -1] |];
          right_edges = [(0, 0, 3); (1, 1, 3); (2, 2, 3); (0, 1, 1); (1, 2, 2)];
          apply_cond = [];
          ebd_expr =
            [ ( 0,
                "point",
                Plus
                  ( Scal (0.7, PointEbd (Node 0)),
                    Scal (0.3, Mean (OrbitPoint ([0; 1; 3], Node 0))) ) );
              (0, "color", Mean (OrbitColor ([0; 2], Node 0)));
              (2, "color", Mean (OrbitColor ([1; 2], Node 0))) ]
        } );
      ( "global edge subdivision",
        { hooks = [0];
          left_nodes = [| [0; 1; 2; 3] |];
          left_edges = [];
          node_match = [| 0 |];
          right_nodes = [| [-1; 1; 2; 3]; [1; -1; 2; 3] |];
          right_edges = [(0, 1, 0)];
          apply_cond = [];
          ebd_expr = [(1, "point", Mean (OrbitPoint ([0], Node 0)))]
        } );
      ( "global face subdivision",
        { hooks = [0];
          left_nodes = [| [0; 1; 2; 3] |];
          left_edges = [];
          node_match = [| 0 |];
          right_nodes =
            [| [-1; 1; 2; 3]; [-1; -1; 2; 3]; [2; -1; -1; 3]; [2; 1; -1; 3] |];
          right_edges = [(0, 1, 0); (1, 2, 1); (2, 3, 0)];
          apply_cond = [];
          ebd_expr =
            [ (1, "point", Mean (OrbitPoint ([0], Node 0)));
              (3, "point", Mean (OrbitPoint ([0; 1], Node 0))) ]
        } );
      ( "global volume subdivision",
        { hooks = [0];
          left_nodes = [| [0; 1; 2; 3] |];
          left_edges = [];
          node_match = [| 0 |];
          right_nodes =
            [| [-1; 1; 2; 3];
               [-1; -1; 2; 3];
               [-1; -1; -1; 3];
               [-1; 1; -1; 3];
               [3; -1; 1; -1];
               [3; -1; -1; -1];
               [3; 2; -1; -1];
               [3; 2; 1; -1]
            |];
          right_edges =
            [ (0, 1, 0);
              (1, 2, 1);
              (2, 3, 0);
              (2, 4, 2);
              (3, 5, 2);
              (4, 5, 0);
              (5, 6, 1);
              (6, 7, 0) ];
          apply_cond = [];
          ebd_expr =
            [ (1, "point", Mean (OrbitPoint ([0], Node 0)));
              (3, "point", Mean (OrbitPoint ([0; 1], Node 0)));
              (7, "point", Mean (OrbitPoint ([0; 1; 2], Node 0)));
              (4, "color", Mean (OrbitColor ([0; 1; 2], Node 0))) ]
        } );
      ( "vertex removal",
        { hooks = [1];
          left_nodes = [| [2; 3]; [2; 3]; [2; 3]; [2; 3] |];
          left_edges = [(0, 1, 0); (1, 2, 1); (2, 3, 0)];
          node_match = [| 0; -1; -1; 1 |];
          right_nodes = [| [2; 3]; [2; 3] |];
          right_edges = [(0, 1, 0)];
          apply_cond = [];
          ebd_expr = []
        } );
      ( "edge removal",
        { hooks = [1];
          left_nodes = [| [-1; 3]; [0; 3]; [0; 3]; [-1; 3] |];
          left_edges = [(0, 1, 1); (1, 2, 2); (2, 3, 1)];
          node_match = [| 0; -1; -1; 1 |];
          right_nodes = [| [-1; 3]; [-1; 3] |];
          right_edges = [(0, 1, 1)];
          apply_cond = [];
          ebd_expr = []
        } );
      ( "face removal",
        { hooks = [1];
          left_nodes = [| [0; -1]; [0; 1]; [0; 1]; [0; -1] |];
          left_edges = [(0, 1, 2); (1, 2, 3); (2, 3, 2)];
          node_match = [| 0; -1; -1; 1 |];
          right_nodes = [| [0; -1]; [0; -1] |];
          right_edges = [(0, 1, 2)];
          apply_cond = [];
          ebd_expr = []
        } );
      ( "menger sponge",
        { hooks = [0];
          left_nodes = [| [0; 1; 2; 3] |];
          left_edges = [];
          node_match = [| 0 |];
          right_nodes =
            [| [-1; 1; 2; 3];
               [-1; -1; 2; 3];
               [-1; -1; -1; 3];
               [-1; 1; -1; 3];
               [-1; -1; 1; -1];
               [-1; -1; -1; -1];
               [-1; 2; -1; -1];
               [-1; 2; 1; -1];
               [-1; -1; 1; -1];
               [-1; -1; -1; -1];
               [-1; -1; -1; -1];
               [-1; -1; 1; -1];
               [-1; -1; -1; 3];
               [-1; -1; -1; 3];
               [-1; -1; -1; -1];
               [-1; -1; -1; -1];
               [0; -1; 2; 3];
               [0; -1; -1; 3];
               [0; -1; -1; -1];
               [0; -1; 2; -1]
            |];
          right_edges =
            [ (0, 1, 0);
              (1, 2, 1);
              (2, 3, 0);
              (4, 5, 0);
              (5, 6, 1);
              (6, 7, 0);
              (8, 9, 0);
              (9, 10, 1);
              (10, 11, 0);
              (12, 13, 0);
              (14, 15, 0);
              (17, 18, 2);
              (2, 4, 2);
              (3, 5, 2);
              (4, 8, 3);
              (5, 9, 3);
              (6, 10, 3);
              (7, 11, 3);
              (8, 12, 2);
              (9, 13, 2);
              (10, 14, 2);
              (11, 15, 2);
              (12, 16, 1);
              (13, 17, 1);
              (14, 18, 1);
              (15, 19, 1);
              (14, 14, 3);
              (15, 15, 3);
              (18, 18, 3);
              (19, 19, 3) ];
          apply_cond = [];
          ebd_expr =
            [ ( 1,
                "point",
                Plus
                  ( Scal (0.4, PointEbd (Node 0)),
                    Scal (0.6, Mean (OrbitPoint ([0], Node 0))) ) );
              ( 3,
                "point",
                Plus
                  ( Scal (0.4, PointEbd (Node 0)),
                    Scal (0.6, Mean (OrbitPoint ([0; 1], Node 0))) ) );
              ( 7,
                "point",
                Plus
                  ( Scal (0.4, PointEbd (Node 0)),
                    Scal (0.6, Mean (OrbitPoint ([0; 1; 2], Node 0))) ) );
              (4, "color", ColorEbd (Node 0));
              (12, "color", ColorEbd (Node 0));
              ( 14,
                "color",
                Plus
                  ( Scal (0.5, ColorEbd (Node 0)),
                    Scal (0.5, Mean (OrbitColor ([0; 1; 2], Node 0))) ) ) ]
        } );
      ( "color left shift",
        { hooks = [0];
          left_nodes = [| [0; 1; 3] |];
          left_edges = [];
          node_match = [| 0 |];
          right_nodes = [| [0; 1; 3] |];
          right_edges = [];
          apply_cond = [];
          ebd_expr = [(0, "color", LeftShift (ColorEbd (Node 0)))]
        } );
      ( "color right shift",
        { hooks = [0];
          left_nodes = [| [0; 1; 3] |];
          left_edges = [];
          node_match = [| 0 |];
          right_nodes = [| [0; 1; 3] |];
          right_edges = [];
          apply_cond = [];
          ebd_expr = [(0, "color", RightShift (ColorEbd (Node 0)))]
        } );
      ( "non-triangle face triangulation",
        { hooks = [0];
          left_nodes = [| [0; 1; 3] |];
          left_edges = [];
          node_match = [| 0 |];
          right_nodes = [| [0; -1; 3]; [-1; 2; 3]; [1; 2; 3] |];
          right_edges = [(0, 1, 1); (1, 2, 0)];
          apply_cond = [SubOrbit ([0; 1], [0], 0, fun x -> x > 3)];
          ebd_expr =
            [ (2, "point", Mean (OrbitPoint ([0; 1; 3], Node 0)));
              ( 0,
                "color",
                Mean (EnsExpr [ColorEbd (Node 0); ColorEbd (Alpha2 (Node 0))])
              ) ]
        } );
      ( "connect alpha3 sewing",
        { hooks = [0; 1];
          left_nodes = [| [0; 1]; [0; 1] |];
          left_edges = [(0, 0, 3); (1, 1, 3)];
          node_match = [| 0; 1 |];
          right_nodes = [| [0; 1]; [0; 1] |];
          right_edges = [(0, 1, 3)];
          apply_cond = [SameOrbit ([0; 2; 3], 0, 1)];
          ebd_expr =
            [(1, "point", PointEbd (Node 0)); (1, "color", ColorEbd (Node 0))]
        } ) ]
