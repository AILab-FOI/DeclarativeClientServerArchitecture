import { Component, inject, input } from '@angular/core';
import { Kolegij, UserService } from '../../core';
import { Router } from '@angular/router';

@Component({
  selector: 'app-course-card',
  standalone: true,
  imports: [],
  templateUrl: './course-card.component.html',
  styleUrl: './course-card.component.scss',
})
export class CourseCardComponent {
  public course = input.required<Kolegij>();
  public router = inject(Router);
  public user = inject(UserService);

  public goTo(): void {
    this.router.navigate([
      `${this.user.routePrefix()}course`,
      this.course().id,
    ]);
  }
}
