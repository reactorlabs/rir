
advance <- function(dt) {
    dxx <- matrix(0, n_bodies, n_bodies)
    dyy <- matrix(0, n_bodies, n_bodies)
    dzz <- matrix(0, n_bodies, n_bodies)
    for (i in 1:n_bodies) {
        for (j in 1:n_bodies) {
            dxx[[i, j]] <- body_x[[i]] - body_x[[j]]
            dyy[[i, j]] <- body_y[[i]] - body_y[[j]]
            dzz[[i, j]] <- body_z[[i]] - body_z[[j]]
        }
    }

    for (i in 1:(n_bodies - 1)) {
        j_from <- min(i + 1, n_bodies)
        for (j in j_from:n_bodies) {
            dx <- body_x[[i]] - body_x[[j]]
            dy <- body_y[[i]] - body_y[[j]]
            dz <- body_z[[i]] - body_z[[j]]
            distance <- sqrt(dx * dx + dy * dy + dz * dz)
            mag <- dt / (distance * distance * distance)
            body_vx[[i]] <<- body_vx[[i]] - dx * body_mass[[j]] * mag
            body_vy[[i]] <<- body_vy[[i]] - dy * body_mass[[j]] * mag
            body_vz[[i]] <<- body_vz[[i]] - dz * body_mass[[j]] * mag
            body_vx[[j]] <<- body_vx[[j]] + dx * body_mass[[i]] * mag
            body_vy[[j]] <<- body_vy[[j]] + dy * body_mass[[i]] * mag
            body_vz[[j]] <<- body_vz[[j]] + dz * body_mass[[i]] * mag
        }
    }

    for (i in 1:n_bodies) {
        body_x[[i]] <<- body_x[[i]] + dt * body_vx[[i]]
        body_y[[i]] <<- body_y[[i]] + dt * body_vy[[i]]
        body_z[[i]] <<- body_z[[i]] + dt * body_vz[[i]]
    }
}

rir.compile(advance)
rir.disassemble(advance)
